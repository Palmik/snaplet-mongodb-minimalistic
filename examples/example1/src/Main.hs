{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Exception (SomeException, try)

import qualified Data.Text as T

import           Snap.Http.Server
import           Snap.Snaplet
import           Snap.Core
import           Snap.Snaplet.Config
import           System.IO

import           Site

#ifdef DEVELOPMENT
import           Snap.Loader.Dynamic
#else
import           Snap.Loader.Static
#endif

main :: IO ()
main = do
    (conf, site, cleanup) <- $(loadSnapTH [| getConf |]
                                          'getActions
                                          ["resources/templates"])

    _ <- try $ httpServe conf $ site :: IO (Either SomeException ())
    cleanup

getConf :: IO (Config Snap AppConfig)
getConf = commandLineConfig defaultConfig


-- | This function generates the the site handler and cleanup action
-- from the configuration.  In production mode, this action is only
-- run once.  In development mode, this action is run whenever the
-- application is reloaded.
--
-- Development mode also makes sure that the cleanup actions are run
-- appropriately before shutdown.  The cleanup action returned from
-- loadSnapTH should still be used after the server has stopped
-- handling requests, as the cleanup actions are only automatically
-- run when a reload is triggered.
--
-- This sample doesn't actually use the config passed in, but more
-- sophisticated code might.
getActions :: Config Snap AppConfig -> IO (Snap (), IO ())
getActions conf = do
    (msgs, site, cleanup) <- runSnaplet (appEnvironment =<< getOther conf) app
    hPutStrLn stderr $ T.unpack msgs
    return (site, cleanup)
