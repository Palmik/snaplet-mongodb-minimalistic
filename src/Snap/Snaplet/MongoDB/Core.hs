{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Snap.Snaplet.MongoDB.Core
( MongoDB(..)
, HasMongoDB(..)
, mongoDBInit
) where

import            Data.Text (Text)

import            Snap

import            Database.MongoDB
import            System.IO.Pool

------------------------------------------------------------------------------
-- |
description :: Text
description = "Minimalistic MongoDB Snaplet"

------------------------------------------------------------------------------
-- |
data MongoDB = MongoDB
    { mongoPool     :: Pool IOError Pipe
    , mongoDatabase :: Database
    }

------------------------------------------------------------------------------
-- |
class HasMongoDB app where
    getMongoDB :: app -> MongoDB

------------------------------------------------------------------------------
-- |
mongoDBInit :: Int -> Host -> Database -> SnapletInit app MongoDB
mongoDBInit n h d = makeSnaplet "snaplet-mongodb" description Nothing $ do
    pool <- liftIO $ newPool (Factory (connect h) close isClosed) n
    return $ MongoDB pool d
