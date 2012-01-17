{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Snap.Snaplet.MongoDB.Functions.S
( eitherWithDB'
, eitherWithDB
, maybeWithDB
, maybeWithDB'
, unsafeWithDB
, unsafeWithDB'
) where

import           Data.Text (Text)
import           Control.Monad.Error

import           Snap
import           Snap.Snaplet.MongoDB.Core

import           Database.MongoDB
import           System.IO.Pool

import qualified Control.Category as C ((.))

------------------------------------------------------------------------------
-- | These classes are here just for convenience.
class (MonadIO m, MonadState app m) => MonadState' app m
instance (MonadIO m, MonadState app m) => MonadState' app m

class (MonadState' app m, HasMongoDB app) => HasMongoDB' app m
instance (MonadState' app m, HasMongoDB app) => HasMongoDB' app m

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
