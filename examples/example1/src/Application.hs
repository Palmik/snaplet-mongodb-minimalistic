{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Application where

import Data.Lens.Template
import Data.Lens.Common

import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.MongoDB.Core

import Control.Category ((.))
import Prelude hiding ((.))

data App = App
    { _heist :: Snaplet (Heist App)
    , _database :: Snaplet MongoDB
    }

type AppHandler = Handler App App

makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasMongoDB App where
    getMongoDB = getL (snapletValue . database)

