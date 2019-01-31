{-# LANGUAGE OverloadedStrings #-}

module Underpass.Web where

import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe

import Control.Applicative

import Paths_thething

serve :: IO ()
serve = do
    rootFile  <- getDataFileName "data/static/index.html"
    staticDir <- getDataFileName "data/static"
    quickHttpServe (site rootFile staticDir)

site :: FilePath -> FilePath -> Snap ()
site rootFile staticDir =
    ifTop (serveFile rootFile) <|>
    route [ ("foo", writeBS "bar")
         , ("echo/:echoparam", echoHandler)
         , ("q/:query", queryHandler)
         ] <|>
    dir "static" (serveDirectory staticDir)

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param

queryHandler :: Snap ()
queryHandler = do
    (Just query) <- getParam "query"

    writeBS $ "fu" <> query
