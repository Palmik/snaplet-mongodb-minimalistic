{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Snap.Snaplet.MongoDB.Functions
( mongoDBInit
, eitherWithDB'
, eitherWithDB
, maybeWithDB
, maybeWithDB'
, unsafeWithDB
, unsafeWithDB'
) where

import            Data.Text (Text)
import            Control.Monad.Error

import            Snap
import            Snap.Snaplet.MongoDB.Core

import            Database.MongoDB
import            System.IO.Pool

------------------------------------------------------------------------------
-- | Description text used in mongoDBInit as makeSnaplet argument.
description :: Text
description = "Minimalistic MongoDB Snaplet."

------------------------------------------------------------------------------
-- | Initializer function.
-- 1. argument: Maximum pool size.
-- 2. argument: Host (e.g. return value of MongoDB's host function).
-- 3. argument: Database name.
--
-- Example:
-- @
-- app :: SnapletInit App App
-- app = makeSnaplet "app" "An snaplet example application." Nothing $ do
--     d <- nestSnaplet "database" database $ mongoDBInit 10 (host "127.0.0.1") "Snaplet-MongoDB"
--     return $ App d
-- @
mongoDBInit :: Int -> Host -> Database -> SnapletInit app MongoDB
mongoDBInit n h d = makeSnaplet "snaplet-mongodb" description Nothing $ do
    pool <- liftIO $ newPool (Factory (connect h) close isClosed) n
    return $ MongoDB pool d

class (MonadIO m, MonadState app m, HasMongoDB app) => HasMongoDB' app m
instance (MonadIO m, MonadState app m, HasMongoDB app) => HasMongoDB' app m

------------------------------------------------------------------------------
-- | Database access function.
-- 1. argument: Action to perform. (Defaults to UnconfirmedWrites AccessMode)
-- Returns: The action's result; in case of failure error is called.
--
-- Example:
-- > unsafeWithDB $ insert "test-collection" ["some_field" = " something" ]
unsafeWithDB :: (HasMongoDB' app m) => Action IO a -> m a
unsafeWithDB = unsafeWithDB' UnconfirmedWrites

------------------------------------------------------------------------------
-- | Database access function.
-- 1. argument: AccessMode.
-- 2. argument: Action to perform.
-- Returns: The action's result; in case of failure error is called.
--
-- Example:
-- > unsafeWithDB' UnconfirmedWrites $ insert "test-collection" ["some_field" = " something" ]
unsafeWithDB' :: (HasMongoDB' app m) => AccessMode -> Action IO a -> m a
unsafeWithDB' mode action = do
    res <- (eitherWithDB' mode action)
    return $ either (error . show) id res

------------------------------------------------------------------------------
-- | Database access function.
-- 1. argument: Action to perform. (Defaults to UnconfirmedWrites AccessMode)
-- Returns: Nothing in case of failure or Just the rsult of the action.
--
-- Example:
-- > maybeWithDB $ insert "test-collection" ["some_field" = " something" ]
maybeWithDB :: (HasMongoDB' app m) => Action IO a -> m (Maybe a)
maybeWithDB = maybeWithDB' UnconfirmedWrites

------------------------------------------------------------------------------
-- | Database access function.
-- 1. argument: AccessMode.
-- 2. argument: Action to perform.
-- Returns: Nothing in case of failure or Just the rsult of the action.
--
-- Example:
-- > maybeWithDB' UnconfirmedWrites $ insert "test-collection" ["some_field" = " something" ]
maybeWithDB' :: (HasMongoDB' app m) => AccessMode -> Action IO a -> m (Maybe a)
maybeWithDB' mode action = do
    res <- (eitherWithDB' mode action)
    return $ either (const Nothing) Just res

------------------------------------------------------------------------------
-- | Database access function.
-- 1. argument: Action to perform. (Defaults to UnconfirmedWrites AccessMode)
-- Returns: Either Failure or the action's result.
--
-- Example:
-- > eitherWithDB $ insert "test-collection" ["some_field" = " something" ]
eitherWithDB :: (HasMongoDB' app m) => Action IO a -> m (Either Failure a)
eitherWithDB = eitherWithDB' UnconfirmedWrites

------------------------------------------------------------------------------
-- | Database access function.
-- 1. argument: AccessMode.
-- 2. argument: Action to perform.
-- Returns: Either Failure or the action's result.
--
-- Example:
-- > eitherWithDB' UnconfirmedWrites $ insert "test-collection" ["some_field" = " something" ]
eitherWithDB' :: (HasMongoDB' app m) => AccessMode -> Action IO a -> m (Either Failure a)
eitherWithDB' mode action = do
    (MongoDB pool database) <- gets getMongoDB
    ep <- liftIO $ runErrorT $ aResource pool
    case ep of
         Left  err -> return $ Left $ ConnectionFailure err
         Right pip -> liftIO $ access pip mode database action
         