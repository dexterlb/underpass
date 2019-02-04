{-# LANGUAGE OverloadedStrings #-}

module Underpass.Web where

import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe

import Data.Aeson (encode)
import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy as BL
import Data.Text.Encoding (decodeUtf8)
import System.Directory (createDirectoryIfMissing)

import Paths_thething

import Underpass.Solution

serve :: UnderpassProgram -> IO ()
serve program = do
    createDirectoryIfMissing True "log" -- must fix this
    rootFile  <- getDataFileName "data/static/index.html"
    staticDir <- getDataFileName "data/static"
    quickHttpServe (site program rootFile staticDir)

site :: UnderpassProgram -> FilePath -> FilePath -> Snap ()
site program rootFile staticDir =
    ifTop (serveFile rootFile) <|>
    route [ ("foo", writeBS "bar")
          , ("q/:query", queryHandler program)
         ] <|>
    dir "static" (serveDirectory staticDir)

queryHandler :: UnderpassProgram -> Snap ()
queryHandler program = do
    (Just query) <- getParam "query"
    sol <- liftIO $ solve program $ decodeUtf8 query
    writeBS $ BL.toStrict $ encode sol
    modifyResponse (setContentType "application/json")
