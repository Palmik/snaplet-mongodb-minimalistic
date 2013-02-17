{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Application where

import Control.Lens
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.MongoDB.Core

data App = App
    { _heist :: Snaplet (Heist App)
    , _database :: Snaplet MongoDB
    }

type AppHandler = Handler App App

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

-- This is ugly, how to beautify it?
instance HasMongoDB App where
    getMongoDB app = view snapletValue (view (database) app)

