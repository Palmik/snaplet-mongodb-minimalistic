{-# LANGUAGE OverloadedStrings     #-}

module Snap.Snaplet.MongoDB.Core
( MongoDB(..)
, HasMongoDB(..)
, mongoDBInit
) where

import           Data.Text (Text)

import           Snap

import           Database.MongoDB
import           System.IO.Pool

------------------------------------------------------------------------------
-- | Description text used in mongoDBInit as makeSnaplet argument.
description :: Text
description = "Minimalistic MongoDB Snaplet."

------------------------------------------------------------------------------
-- | Snaplet's data type.
--
-- Example:
-- 
-- > data App = App
-- >     { _heist :: Snaplet (Heist App)
-- >     , _database :: Snaplet MongoDB
-- >     }
data MongoDB = MongoDB
    { mongoPool     :: Pool IOError Pipe
    , mongoDatabase :: Database
    }
    
------------------------------------------------------------------------------
-- | Snaplet's type-class.
--
-- Example:
--
--
-- > instance HasMongoDB App where
-- >     getMongoDB = getL (snapletValue . database)
--
-- Note: The @(.)@ is from 'Control.Category'.
class HasMongoDB app where
    getMongoDB :: app -> MongoDB

------------------------------------------------------------------------------
-- | Initializer function.
--
-- 1. argument: Maximum pool size.
--
-- 2. argument: Host (e.g. return value of MongoDB's host function).
--
-- 3. argument: Database name.
--
-- Example:
--
-- > app :: SnapletInit App App
-- > app = makeSnaplet "app" "Example application." Nothing $ do
-- >     h <- nestSnaplet "heist" heist $ heistInit "resources/templates"
-- >     d <- nestSnaplet "database" database $ mongoDBInit 10 (host "127.0.0.1") "Snaplet-MongoDB"
-- >     return $ App h d
mongoDBInit :: Int -> Host -> Database -> SnapletInit app MongoDB
mongoDBInit n h d = makeSnaplet "snaplet-mongodb" description Nothing $ do
    pool <- liftIO $ newPool (Factory (connect h) close isClosed) n
    return $ MongoDB pool d
    