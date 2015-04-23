{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Web.Apiary
import Network.Wai (Request, lazyRequestBody)
import Network.Wai.Handler.Warp (run, Port)
import qualified Network.Wai.Parse as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (decodeUtf8)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Time.Clock
import Data.Either
import Codec.Digest.SHA
import Rainbow
import Control.Monad.Apiary.Action
import Control.Monad
import Control.Exception.Base
import GHC.IO.Handle
import GHC.IO.Handle.FD
import System.Environment
import System.FilePath
import System.Directory
import System.IO
import System.IO.Error

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
            r <- liftIO $ tryIOError $ do 
                createDirectoryIfMissing True dir
                getPermissions dir
            contentType "text/plain" 
            case r of 
                Right p -> when (writable p) $ do
                    liftIO $ logOperation "Sync client detected" dir
                    bytes "hsync server 0.1"
                Left e  -> liftIO $ logError "Client refused" e

        method GET . document "Read file at given path" . action $ do
            p <- getFilePath
            liftIO $ logOperation "Read" p
            devFile p

        method DELETE . document "Remove file at given path" . action $ do
            p <- getFilePath
            liftIO $ do
                r <- tryIOError $ removeFile p
                case r of Right _ -> logOperation "Remove" p
                          Left  e -> logError "Remove failed" e

        method PUT . document "Write/Create file at given path" . action $ do
            p <- getFilePath
            contentType "text/plain"
            writePutFile p =<< getRequest

        method POST . document "Write/Create file to given folder" . action $ do
            dir <- getFilePath
            contentType "text/plain"
            writePostFiles dir =<< getRequest

getFilePath :: ActionT exts '["path" ':= [Text]] IO FilePath
getFilePath = do
    p <- param [key|path|]
    return $ combine "/" (joinPath $ map T.unpack p)

logOperation :: String -> FilePath -> IO ()
logOperation op file = do
    t <- getCurrentTime
    putStr $ show t ++ " " ++ op ++ " at "
    putChunkLn $ (chunkFromText $ T.pack file ) <> fore green

logError :: String -> IOError -> IO ()
logError op e = do 
    t <- getCurrentTime
    putStr $ show t ++ " " ++ op ++ " because "
    putChunkLn $ (chunkFromText $ T.pack $ show e ) <> fore red

writePostFiles :: MonadIO m => FilePath -> Request -> ActionT exts prms m ()
writePostFiles dir req = do
        r <- liftIO $ tryIOError $ createDirectoryIfMissing True dir
        case r of
            Right _ -> do
                (_, fs) <- liftIO $ P.parseRequestBody backEnd req
                forM_ fs $ \(_, f) -> appendString $ P.fileContent f
            Left  e -> appendString $ show e
    where
        backEnd _ fi io = do
            let fn = T.unpack $ T.decodeUtf8 $ P.fileName fi
            if not $ fn == "\"\""
                then do
                    let p = combine dir fn
                    r <- tryIOError $ do
                        withFile p WriteMode $ \h ->
                            hSetFileSize h 0 >> loop h io
                        verifyFileSHA p
                    case r of 
                        Left  e   -> do
                            loopEmpty io
                            logError "Write failed" e
                            return $ show e
                        Right sha -> do
                            logOperation "Write" p
                            return sha
                else loopEmpty io >> return "No files found"
        loopEmpty io = do
            bs <- io
            if (BS.null bs) then return () else loopEmpty io
        loop h io = do
            bs <- io
            if (BS.null bs) then return ()
                else BS.hPut h bs >> loop h io

writePutFile :: MonadIO m => FilePath -> Request -> ActionT exts prms m ()
writePutFile p req =
    appendString =<< liftIO io
    where 
        io = writeFileBS dir p =<< lazyRequestBody req
        dir = takeDirectory p
        writeFileBS dir p bs = do
            r1 <- tryIOError $ do
                createDirectoryIfMissing True dir
                BL.writeFile p bs
            r2 <- tryIOError $ verifyFileSHA p
            case (r1 >> r2) of 
                Left  e   -> do
                    logError "Write failed" e
                    return $ show e
                Right sha -> do
                    logOperation "Write" p
                    return sha
                

verifyFileSHA :: FilePath -> IO String
verifyFileSHA f = do
    nf <- BL.readFile f
    return $ (showBSasHex $ hash SHA512 nf) ++ "\n"
