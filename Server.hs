{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

import System.Environment
import System.Exit
import Web.Apiary
import Network.Wai.Handler.Warp
import Control.Monad.Apiary.Action
import Control.Monad
import System.IO.Error (catchIOError)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (decodeUtf8)
import qualified Data.ByteString.Lazy as B (writeFile, readFile)
import qualified Data.ByteString as BS (writeFile)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import System.FilePath
import System.Directory
import Rainbow
import Data.Time.Clock
import Codec.Digest.SHA
import Codec.Digest.SHA.Misc

main = getArgs >>= parse
parse ["-h"] = usage >> exit
parse ["-v"] = version >> exit
parse (port:[]) = serve $ read port
parse _ = usage >> exit

usage   = putStrLn "Usage: hsync-server [-vh] [port]"
version = putStrLn "hsync server 0.1"
exit    = exitWith ExitSuccess

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
            succ <- liftIO $ catchIOError
                (createDirectoryIfMissing True dir >> return True) (const False)
            when succ $ do
                logOperation "Sync client detected" dir
                contentType "text/plain" >> bytes "hsync server 0.1"

        method GET . document "Read file at given path" . action $ do
            p <- getFilePath
            logOperation "Read" p
            devFile p

        method DELETE . document "Remove file at given path" . action $ do
            p <- getFilePath
            logOperation "Remove" p
            performOperation $ removeFile p

        method PUT . document "Write/Create file at given path" . action $ do
            p <- getFilePath
            contentType "text/plain"
            Unknown bs <- getReqBody
            writePutFile p bs

        method POST . document "Write/Create file to given folder" . action $ do
            dir <- getFilePath
            contentType "text/plain"
            mapM_ (writePostFile dir) =<< getReqBodyFiles

getFilePath :: ActionT exts '["path" ':= [Text]] IO FilePath
getFilePath = do
    p <- param [key|path|]
    return $ combine "/" (joinPath $ map T.unpack p)

logOperation :: MonadIO m => String -> FilePath -> m ()
logOperation op file = liftIO $ do
    t <- getCurrentTime
    putStr $ show t ++ " " ++ op ++ " at "
    putChunkLn $ (chunkFromText $ T.pack file ) <> fore green

performOperation :: MonadIO m => IO () -> ActionT exts prms m ()
performOperation io = liftIO $ catchIOError io $ putStrLn . show

writePostFile :: MonadIO m => FilePath -> File -> ActionT exts prms m ()
writePostFile dir f = do
    performOperation $ createDirectoryIfMissing True dir
    let fp = combine dir $ T.unpack $ T.decodeUtf8 $ fileName f
    logOperation "Write" fp
    performOperation $ B.writeFile fp $ fileContent f
    verifyfileSHA fp

writePutFile :: MonadIO m => FilePath -> ByteString -> ActionT exts prms m ()
writePutFile p bs = do
    performOperation $ createDirectoryIfMissing True $ takeDirectory p
    logOperation "Write" p
    performOperation $ BS.writeFile p $ bs
    verifyfileSHA p

verifyfileSHA :: MonadIO m => FilePath -> ActionT exts prms m ()
verifyfileSHA f = do
    nf <- liftIO $ B.readFile f
    appendString $ (showBSasHex $ hash SHA512 nf) ++ "\n"
