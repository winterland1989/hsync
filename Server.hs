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
            makeSureDirectory dir $ do
                logOperation "Sync client detected" dir
                return "hsync server 0.1"

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
            req <- getRequest
            makeSureDirectory (takeDirectory p) $ writePutFile p req

        method POST . document "Write/Create file to given folder" . action $ do
            dir <- getFilePath
            contentType "text/plain"
            req <- getRequest
            makeSureDirectory dir $ writePostFiles dir req

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
                        
makeSureDirectory :: MonadIO m => FilePath -> IO String -> ActionT exts prms m ()
makeSureDirectory dir io = do
    r <- liftIO $ tryIOError $ do
        createDirectoryIfMissing True dir
        p <- getPermissions dir
        if (writable p) then return ()
                        else ioError $
                            mkIOError permissionErrorType "during test" Nothing $ Just dir
    case r of
        Right _ -> appendString =<< liftIO io
        Left  e -> do
            liftIO $ logError "Create directory failed" e
            appendString $ show e

writePostFiles :: FilePath -> Request -> IO String
writePostFiles dir req = do
        (_, fs) <- P.parseRequestBody backEnd req
        return $ concat $ map (\(_, f) -> P.fileContent f) fs 
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

writePutFile :: FilePath -> Request -> IO String
writePutFile p req = do
    r <- tryIOError $ do
        BL.writeFile p =<< lazyRequestBody req
        verifyFileSHA p
    case r of 
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
