{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import System.Directory
import System.IO.Error (catchIOError)
import System.FilePath
import Control.Monad.IO.Class
import Control.Monad
import Control.Concurrent (threadDelay)
import Network.Http.Client
import System.FSNotify
import qualified Data.ByteString as B hiding (pack)
import qualified Data.ByteString.Char8 as B (pack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (decodeUtf8)

import Data.String (fromString)
import Data.Time.Clock
import qualified Filesystem.Path as P (FilePath)
import Filesystem.Path.CurrentOS (encodeString)
import Network.URI
import Rainbow
import Codec.Digest.SHA
import Codec.Digest.SHA.Misc

main :: IO ()
main = getArgs >>= parse

parse ["-h"] = usage
parse ["-v"] = version
parse (addr:[]) = case parseAddr addr of
    Just (host, port, path) -> testConnection host port path
    Nothing                 -> print "URI failed to parse."
parse _ = usage

usage   = putStrLn "Usage: hsync [-vh] [remote addr]"
version = putStrLn "hsync 0.1"

parseAddr :: String -> Maybe (Hostname, Port, URI)
parseAddr addr = do
    u <- parseURI addr
    ua <- uriAuthority u
    rUri <- parseRelativeReference $ uriPath u
    let port = case uriPort ua of (':':num) -> read num
                                  _         -> 80
    return ( fromString $ uriRegName ua, port, rUri )

printColor c s = putChunkLn $ ( chunkFromText $ T.pack $ show s ) <> fore c
printGreen :: Show s => s -> IO ()
printGreen     = printColor green
printRed :: Show s => s -> IO ()
printRed       = printColor red

testConnection :: Hostname -> Port -> URI -> IO ()
testConnection host port uri = do
    c <- openConnection host port
    req <- buildRequest $ http OPTIONS (fromString $ show uri)
    sendRequest c req emptyBody
    opt <- receiveResponse c concatHandler
    if B.take 12 opt == "hsync server"
        then do
            cd <- getCurrentDirectory
            putStr "Start sync service at: " >> printGreen cd
            putStr "Remote HOST: " >> printGreen host
            putStr "Remote PORT: " >> printGreen port
            putStr "Remote PATH: " >> printGreen uri
            startMonitor c uri cd
        else putStrLn "Can't acess remote path" >> closeConnection c

fNConfig :: WatchConfig
fNConfig = WatchConfig { confDebounce     = Debounce 0.5
                       , confPollInterval = 500
                       , confUsePolling   = False
                       }

startMonitor :: Connection -> URI -> FilePath -> IO ()
startMonitor c uri cd = do 
    withManagerConf fNConfig $ \mgr -> do
        watchTree mgr "." (const True) (sync c uri cd)
        -- sleep forever (until interrupted)
        forever $ threadDelay maxBound

sync :: Connection -> URI -> FilePath -> Event -> IO ()
sync c uri cd e = case e of
    Removed  p t  -> go' DELETE uri (relativeFilePath p) t
    Added    p t  -> go' PUT    uri (relativeFilePath p) t
    Modified p t  -> go' PUT    uri (relativeFilePath p) t
    where
        relativeFilePath p = makeRelative cd $ encodeString p

        go :: Method -> URI -> FilePath -> UTCTime -> IO ()
        go op uri p t = case parseRelativeReference p of
            Nothing   -> do
                putStr $ show t ++ " " ++ show op ++ " failed "
                printGreen "parse relative path failed"
            Just relativeURI -> do
                let fUri = relativeURI `relativeTo` uri
                putStr $ show t ++ " " ++ show op ++ " at "
                printGreen fUri
                let body = case op of DELETE -> emptyBody
                                      PUT    -> fileBody p
                req <- buildRequest $ http op $ B.pack $ show fUri
                sendRequest c req body
                res <- receiveResponse c concatHandler
                when (op == PUT) $ do
                    putStr "Remote File SHA512: " >> printGreen res
                    nf <- B.readFile p
                    let localHash = B.pack $ (showBSasHex $ hash SHA512 nf) ++ "\n"
                    putStr "Local File SHA512: " >> printGreen localHash

                    if res == localHash
                        then printGreen "File verified OK!"
                        else printRed "File verified failed!"

        go' :: Method -> URI -> FilePath -> UTCTime -> IO ()
        go' op uri p t = do
            putStr $ show t ++ " " ++ show op ++ " at "
            printGreen p
