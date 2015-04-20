{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import System.Exit
import System.Directory
import System.IO.Error (catchIOError)
import System.FilePath
import Control.Monad.IO.Class
import Control.Monad
import Control.Concurrent (threadDelay)
import Network.Http.Client
import qualified System.FSNotify as FN
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

printGreen s = putChunkLn $ ( chunkFromText $ T.pack $ show s ) <> fore green

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
        else do
            putStrLn "hsync server not found" >> closeConnection c


startMonitor :: Connection -> URI -> FilePath -> IO ()
startMonitor c uri cd = do 
    FN.withManager $ \mgr -> do
        FN.watchTree mgr "." (const True) (sync c uri cd)
        -- sleep forever (until interrupted)
        forever $ threadDelay maxBound

sync :: Connection -> URI -> FilePath -> FN.Event -> IO ()
sync c uri cd e = case e of
    FN.Removed  p t  -> go DELETE uri (relativeFilePath p) t
    FN.Added    p t  -> go PUT    uri (relativeFilePath p) t
    FN.Modified p t  -> go PUT    uri (relativeFilePath p) t
    where
        relativeFilePath p = makeRelative cd $ encodeString p
        
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



        encodeFilePath = encodeFilePath

