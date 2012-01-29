{-# LANGUAGE OverloadedStrings     #-}

------------------------------------------------------------------------------
-- | In this module you can find the Snaplet's data type, type class and initializer.
------------------------------------------------------------------------------
module Snap.Snaplet.MongoDB.Core
( MongoDB(..)
, HasMongoDB(..)
, MongoDBPool
, mongoDBInit
) where

import           Data.Text (Text)

import           Snap

import           Database.MongoDB
import           System.IO.Pool

------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Description text used in mongoDBInit as makeSnaplet argument.
description :: Text
description = "Minimalistic MongoDB Snaplet."

------------------------------------------------------------------------------
-- | MongoDB Pool type
type MongoDBPool = Pool IOError Pipe

------------------------------------------------------------------------------
-- | Snaplet's data type.
--
-- Usage:
-- 
-- > data App = App
-- >     { _heist :: Snaplet (Heist App)
-- >     , _database :: Snaplet MongoDB
-- >     }
data MongoDB = MongoDB
    { mongoPool :: Pool IOError Pipe
    , mongoDatabase :: Database
    , mongoAccessMode :: AccessMode
    }
    
------------------------------------------------------------------------------
-- | Snaplet's type-class.
--
-- Usage:
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
-- Usage:
--
-- > app :: SnapletInit App App
-- > app = makeSnaplet "app" "Example application." Nothing $ do
-- >     h <- nestSnaplet "heist" heist $ heistInit "resources/templates"
-- >     d <- nestSnaplet "database" database $ mongoDBInit 10 (host "127.0.0.1") "Snaplet-MongoDB"
-- >     return $ App h d
mongoDBInit :: Int                      -- ^ Maximum pool size.
            -> Host                     -- ^ Host (e.g. return value of MongoDB's host function).
            -> Database                 -- ^ Database name.
            -> SnapletInit app MongoDB
mongoDBInit n h d = makeSnaplet "snaplet-mongodb" description Nothing $ do
    pool <- liftIO $ newPool (Factory (connect h) close isClosed) n
    return $ MongoDB pool d UnconfirmedWrites

------------------------------------------------------------------------------
-- | Initializer function.
--
-- Usage:
--
-- > app :: SnapletInit App App
-- > app = makeSnaplet "app" "Example application." Nothing $ do
-- >     h <- nestSnaplet "heist" heist $ heistInit "resources/templates"
-- >     d <- nestSnaplet "database" database $ mongoDBInit 10 (host "127.0.0.1") "Snaplet-MongoDB"
-- >     return $ App h d
mongoDBInit' :: Int                      -- ^ Maximum pool size.
             -> Host                     -- ^ Host (e.g. return value of MongoDB's host function).
             -> Database                 -- ^ Database name.
             -> AccessMode               -- ^ Default access mode to be used with this snaplet.
             -> SnapletInit app MongoDB
mongoDBInit' n h d m = makeSnaplet "snaplet-mongodb" description Nothing $ do
    pool <- liftIO $ newPool (Factory (connect h) close isClosed) n
    return $ MongoDB pool d m
    