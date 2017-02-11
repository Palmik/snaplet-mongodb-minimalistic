{-# LANGUAGE OverloadedStrings     #-}

------------------------------------------------------------------------------
-- | In this module you can find the Snaplet's data type, type class and initializer.
------------------------------------------------------------------------------
module Snap.Snaplet.MongoDB.Core
( MongoDB(..)
, HasMongoDB(..)
, MongoDBPool
, mongoDBInit
, mongoDBInit'
) where

import           Data.Text (Text)
import           Snap.Snaplet
import           Control.Monad.IO.Class
import           Database.MongoDB (Database, Host, Pipe, AccessMode (..), close, connect)
import           Data.Pool (Pool, createPool)

------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Description text used in mongoDBInit as makeSnaplet argument.
description :: Text
description = "Minimalistic MongoDB Snaplet."

------------------------------------------------------------------------------
-- | MongoDB Pool type
type MongoDBPool = Pool Pipe

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
    { mongoPool :: Pool Pipe
    , mongoDatabase :: Database
    , mongoAccessMode :: AccessMode
    }

------------------------------------------------------------------------------
-- | Snaplet's type-class.
--
-- Usage:
--
-- > instance HasMongoDB App where
-- >     getMongoDB app = view snapletValue (view database app)
class HasMongoDB a where
    getMongoDB :: a -> MongoDB

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
mongoDBInit poolSize host database =
  makeSnaplet "snaplet-mongodb" description Nothing $ do
    pool <- liftIO $ createPool (connect host) close poolSize 0.5 1
    return $ MongoDB pool database (ConfirmWrites [])

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
    pool <- liftIO $ createPool (connect h) close n 50 3 -- TODO I'm not so sure about this numbers
    return $ MongoDB pool d m

