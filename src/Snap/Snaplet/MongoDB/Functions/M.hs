{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Snap.Snaplet.MongoDB.Functions.M
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

------------------------------------------------------------------------------
-- | Database access function.
--
-- 1. argument: The snaplet (database) on which you want the action to be run.
-- 2. argument: Action to perform. (Defaults to UnconfirmedWrites AccessMode)
-- Returns: The action's result; in case of failure error is called.
--
-- Example:
-- > unsafeWithDB accountDB $ insert "test-collection" ["some_field" = "something" ]
unsafeWithDB :: (MonadState' app m) => Lens app (Snaplet MongoDB) -> Action IO a -> m a
unsafeWithDB snaplet = unsafeWithDB' snaplet UnconfirmedWrites

------------------------------------------------------------------------------
-- | Database access function.
-- 1. argument: The snaplet (database) on which you want the action to be run.
-- 2. argument: AccessMode.
-- 3. argument: Action to perform.
-- Returns: The action's result; in case of failure error is called.
--
-- Example:
-- > unsafeWithDB' accountDB UnconfirmedWrites $ insert "test-collection" ["some_field" = "something" ]
unsafeWithDB' :: (MonadState' app m) => Lens app (Snaplet MongoDB) -> AccessMode -> Action IO a -> m a
unsafeWithDB' snaplet mode action = do
    res <- (eitherWithDB' snaplet mode action)
    return $ either (error . show) id res

------------------------------------------------------------------------------
-- | Database access function.
-- 1. argument: The snaplet (database) on which you want the action to be run.
-- 2. argument: Action to perform. (Defaults to UnconfirmedWrites AccessMode)
-- Returns: Nothing in case of failure or Just the rsult of the action.
--
-- Example:
-- > maybeWithDB accountDB $ insert "test-collection" ["some_field" = "something" ]
maybeWithDB :: (MonadState' app m) => Lens app (Snaplet MongoDB) -> Action IO a -> m (Maybe a)
maybeWithDB snaplet = maybeWithDB' snaplet UnconfirmedWrites

------------------------------------------------------------------------------
-- | Database access function.
-- 1. argument: The snaplet (database) on which you want the action to be run.
-- 2. argument: AccessMode.
-- 3. argument: Action to perform.
-- Returns: Nothing in case of failure or Just the rsult of the action.
--
-- Example:
-- > maybeWithDB' accountDB UnconfirmedWrites $ insert "test-collection" ["some_field" = "something" ]
maybeWithDB' :: (MonadState' app m) => Lens app (Snaplet MongoDB) -> AccessMode -> Action IO a -> m (Maybe a)
maybeWithDB' snaplet mode action = do
    res <- (eitherWithDB' snaplet mode action)
    return $ either (const Nothing) Just res

------------------------------------------------------------------------------
-- | Database access function.
-- 1. argument: The snaplet (database) on which you want the action to be run.
-- 2. argument: Action to perform. (Defaults to UnconfirmedWrites AccessMode)
-- Returns: Either Failure or the action's result.
--
-- Example:
-- > eitherWithDB accountDB $ insert "test-collection" ["some_field" = "something" ]
eitherWithDB :: (MonadState' app m) => Lens app (Snaplet MongoDB) -> Action IO a -> m (Either Failure a)
eitherWithDB snaplet = eitherWithDB' snaplet UnconfirmedWrites

------------------------------------------------------------------------------
-- | Database access function.
-- 1. argument: The snaplet (database) on which you want the action to be run.
-- 2. argument: AccessMode.
-- 3. argument: Action to perform.
-- Returns: Either Failure or the action's result.
--
-- Example:
-- > eitherWithDB' accountDB UnconfirmedWrites $ insert "test-collection" ["some_field" = "something" ]
eitherWithDB' :: (MonadState' app m) => Lens app (Snaplet MongoDB) -> AccessMode -> Action IO a -> m (Either Failure a)
eitherWithDB' snaplet mode action = do
    (MongoDB pool database) <- gets (getL ((C..) snapletValue snaplet))
    ep <- liftIO $ runErrorT $ aResource pool
    case ep of
         Left  err -> return $ Left $ ConnectionFailure err
         Right pip -> liftIO $ access pip mode database action


