{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

import System.Environment
import Web.Apiary
import Network.Wai (Request, lazyRequestBody)
import Network.Wai.Handler.Warp (run, Port)
import Network.Wai.Parse as P
import Control.Monad.Apiary.Action
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (decodeUtf8)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import System.FilePath
import System.Directory
import Rainbow
import Data.Time.Clock
import Codec.Digest.SHA
import Codec.Digest.SHA.Misc
import Data.Either
import Control.Exception.Base
import GHC.IO.Handle
import GHC.IO.Handle.FD
import System.IO
import System.IO.Error (catchIOError)

main = getArgs >>= parse
parse ["-h"] = usage
parse ["-v"] = version
parse (port:[]) = serve $ read port
parse _ = usage

usage   = putStrLn "Usage: hsync-server [-vh] [port]"
version = putStrLn "hsync server 0.1"

serve :: Port -> IO ()
serve p = runApiary (run p) def $ do

    root . method GET . action $ defaultDocumentationAction def 
            {   documentTitle       = "HSYNC DOC" 
            ,   documentUseCDN      = False
            ,   documentDescription = Just $
                    H.form H.! A.method "POST" H.! A.target "_blank" H.! A.enctype "multipart/form-data"
                        H.! A.onsubmit "this.action=this.lastChild.value;" $ mconcat
                    [   "Upload file to path below:"
                    ,   H.input H.! A.name "files[]" H.! A.multiple "" H.! A.type_ "file"
                    ,   H.input H.! A.style "width:100%" H.! A.value "home/..."
                    ]
            }
    [capture|/static/api-documentation.js|] . action $ file "static/api-documentation.js" Nothing
    [capture|/static/api-documentation.css|] . action $ file "static/api-documentation.css" Nothing

    [capture|/**path[remote path or folder]|] $ do

        method OPTIONS . document "API for client detecting hsync service" . action $ do
            dir <- getFilePath
            r <- performOperation $ createDirectoryIfMissing True dir
            when (isRight r) $ do
                p <- liftIO $ getPermissions dir
                when (writable p) $ do
                    logOperation "Sync client detected" dir
                    contentType "text/plain" >> bytes "hsync server 0.1"

        method GET . document "Read file at given path" . action $ do
            p <- getFilePath
            logOperation "Read" p
            devFile p

        method DELETE . document "Remove file at given path" . action $ do
            p <- getFilePath
            r <- performOperation $ removeFile p
            case r of Right () -> logOperation "Remove" p
                      Left e   -> logError "Remove failed" e

        method PUT . document "Write/Create file at given path" . action $ do
            p <- getFilePath
            contentType "text/plain"
            writePutFile p =<< getRequest

        method POST . document "Write/Create file to given folder" . action $ do
            dir <- getFilePath
            contentType "text/plain"
            writePostFile dir =<< getRequest

getFilePath :: ActionT exts '["path" ':= [Text]] IO FilePath
getFilePath = do
    p <- param [key|path|]
    return $ combine "/" (joinPath $ map T.unpack p)

logOperation :: String -> FilePath -> IO ()
logOperation op file = do
    t <- getCurrentTime
    putStr $ show t ++ " " ++ op ++ " at "
    putChunkLn $ (chunkFromText $ T.pack file ) <> fore green

logError :: MonadIO m => String -> String -> m ()
logError op e =  liftIO $ do 
    t <- getCurrentTime
    putStr $ show t ++ " " ++ op ++ " because "
    putChunkLn $ (chunkFromText $ T.pack e ) <> fore red

performOperation :: MonadIO m => IO () -> ActionT exts prms m (Either String ())
performOperation io = liftIO $ catchIOError succ err
    where succ = io >> (return $ Right ())
          err  = return . Left . show

writeFileBS :: MonadIO m => FilePath -> FilePath -> BL.ByteString -> ActionT exts prms m ()
writeFileBS dir p bs = do
    r <- performOperation $ do 
        createDirectoryIfMissing True dir
        BL.writeFile p bs
    case r of
        Left e  -> logError "Write failed" e
        Right _ -> logOperation "Write" p

writePostFile :: MonadIO m => FilePath -> Request -> ActionT exts prms m ()
writePostFile dir req = do
        (_, fs) <- liftIO $ P.parseRequestBody backEnd req
        forM_ fs $ \(_, f) -> appendString $ P.fileContent f
    where
        backEnd _ fi io = do
            let p = combine dir $ T.unpack $ T.decodeUtf8 $ P.fileName fi
            withFile p WriteMode $ \h -> hSetFileSize h 0 >> loop h io
            verifyfileSHA p
        loop h io = do
            bs <- io
            if (BS.null bs) then return ()
                else BS.hPut h bs >> loop h io

writePutFile :: MonadIO m => FilePath -> Request -> ActionT exts prms m ()
writePutFile p req = writeFileBS dir p =<< liftIO (lazyRequestBody req)
    where dir = takeDirectory p

verifyfileSHA :: FilePath -> IO String
verifyfileSHA f = do
    nf <- BL.readFile f
    return $ (showBSasHex $ hash SHA512 nf) ++ "\n"
