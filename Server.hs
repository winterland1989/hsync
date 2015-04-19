{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

import Web.Apiary
import Network.Wai.Handler.Warp
import Control.Monad.Apiary.Action
import Control.Monad
import System.IO.Error (catchIOError)
import Data.Monoid
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B (writeFile, readFile)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import System.FilePath (combine, joinPath)
import System.Directory (removeFile)
import Rainbow
import Data.Time.Clock
import Codec.Digest.SHA
import Codec.Digest.SHA.Misc

getFilePath = do
    p <- param [key|path|]
    return $ combine "/" (joinPath $ map T.unpack p)

logFileOperation op file = liftIO $ do
    t <- getCurrentTime
    putStr $ show t ++ " " ++ op <> " file at "
    putChunkLn $ (chunkFromText $ T.pack file ) <> fore green

performFileOperation io = liftIO $ catchIOError io $ putStrLn . show

main :: IO ()
main = runApiary (run 3000) def $ do

        -- add documentation page route.
        root . action $
            defaultDocumentationAction def 
                {   documentTitle       = "HSYNC DOC" 
                ,   documentUseCDN      = False
                ,   documentDescription = Just $
                        H.form H.! A.method "POST" H.! A.target "_blank" H.! A.enctype "multipart/form-data"
                            H.! A.onsubmit "this.action=this.childNodes[1].value;" $ mconcat
                        [   "Upload/update file to path below:"
                        ,   H.input H.! A.name "file to upload" H.! A.type_ "file"
                        ,   H.input H.! A.style "width:100%" H.! A.value "home/bae/..."
                        ]
                }
        [capture|/static/api-documentation.js|] . action $ file "static/api-documentation.js" Nothing
        [capture|/static/api-documentation.css|] . action $ file "static/api-documentation.css" Nothing

        [capture|/**path[file path]|] $ do
            method GET . document "Read file api" . action $ do
                p <- getFilePath
                logFileOperation "Read" p
                devFile p

            method DELETE . document "Remove file api" . action $ do
                p <- getFilePath
                logFileOperation "Remove" p
                performFileOperation $ removeFile p

            method POST . document "Write/Create file api" . action $ do
                p <- getFilePath
                f <- getReqBodyFiles
                unless (null f) $ do
                    logFileOperation "Write" p
                    performFileOperation $ B.writeFile p (fileContent $ head f)
                    nf <- liftIO $ B.readFile p 
                    contentType "text/html"
                    string $ showBSasHex $ hash SHA512 nf


