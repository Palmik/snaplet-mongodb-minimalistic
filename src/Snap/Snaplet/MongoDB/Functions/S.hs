{-# LANGUAGE OverloadedStrings     #-}

------------------------------------------------------------------------------
-- | In this module you can find variations of @withDB@ functions.
--
-- Functions from this module are to be used when you have single MongoDB snaplet in your application and your application is an instance of HasMongoDB.
------------------------------------------------------------------------------
module Snap.Snaplet.MongoDB.Functions.S
( eitherWithDB
, eitherWithDB'
, maybeWithDB
, maybeWithDB'
, unsafeWithDB
, unsafeWithDB'
) where

import           Control.Monad.Error (runErrorT)
import           Snap
import           Snap.Snaplet.MongoDB.Core

import           Database.MongoDB (Action, AccessMode, Failure (ConnectionFailure), access)
import           System.IO.Pool (aResource)

------------------------------------------------------------------------------
-- | Database access function.
--
-- Usage:
--
-- > unsafeWithDB $ insert "test-collection" [ "some_field" = "something" ]
unsafeWithDB :: (MonadIO m, MonadState app m, HasMongoDB app)
             => Action IO a         -- ^ 'Action' you want to perform.
             -> m a                 -- ^ The action's result; in case of failure 'error' is called.
unsafeWithDB action = getMongoAccessMode >>= flip unsafeWithDB' action

------------------------------------------------------------------------------
-- | Database access function.
--
-- Usage:
--
-- > unsafeWithDB' UnconfirmedWrites $ insert "test-collection" [ "some_field" = "something" ]
unsafeWithDB' :: (MonadIO m, MonadState app m, HasMongoDB app)
              => AccessMode             -- ^ Access mode you want to use when performing the action.
              -> Action IO a            -- ^ 'Action' you want to perform.
              -> m a                    -- ^ The action's result; in case of failure 'error' is called.
unsafeWithDB' mode action = do
    res <- eitherWithDB' mode action
    either (error . show) return res

------------------------------------------------------------------------------
-- | Database access function.
--
-- Usage:
--
-- > maybeWithDB $ insert "test-collection" [ "some_field" = "something" ]
maybeWithDB :: (MonadIO m, MonadState app m, HasMongoDB app)
            => Action IO a          -- ^ 'Action' you want to perform.
            -> m (Maybe a)          -- ^ 'Nothing' in case of failure or 'Just' the result of the action.
maybeWithDB action = getMongoAccessMode >>= flip maybeWithDB' action

------------------------------------------------------------------------------
-- | Database access function.
--
-- Usage:
--
-- > maybeWithDB' UnconfirmedWrites $ insert "test-collection" [ "some_field" = "something" ]
maybeWithDB' :: (MonadIO m, MonadState app m, HasMongoDB app)
             => AccessMode          -- ^ Access mode you want to use when performing the action.
             -> Action IO a         -- ^ 'Action' you want to perform.
             -> m (Maybe a)         -- ^ 'Nothing' in case of failure or 'Just' the result of the action.
maybeWithDB' mode action = do
    res <- eitherWithDB' mode action
    return $ either (const Nothing) Just res

------------------------------------------------------------------------------
-- | Database access function.
--
-- Usage:
--
-- > eitherWithDB $ insert "test-collection" [ "some_field" = "something" ]
eitherWithDB :: (MonadIO m, MonadState app m, HasMongoDB app)
             => Action IO a             -- ^ 'Action' you want to perform.
             -> m (Either Failure a)    -- ^ 'Either' 'Failure' or the action's result.
eitherWithDB action = getMongoAccessMode >>= flip eitherWithDB' action

------------------------------------------------------------------------------
-- | Database access function.
--
-- Usage:
--
-- > eitherWithDB' UnconfirmedWrites $ insert "test-collection" [ "some_field" = "something" ]
eitherWithDB' :: (MonadIO m, MonadState app m, HasMongoDB app)
              => AccessMode             -- ^ Access mode you want to use when performing the action.
              -> Action IO a            -- ^ 'Action' you want to perform.
              -> m (Either Failure a)   -- ^ 'Either' 'Failure' or the action's result.
eitherWithDB' mode action = do
    (MongoDB pool database _) <- gets getMongoDB
    ep <- liftIO $ runErrorT $ aResource pool
    case ep of
         Left  err -> return $ Left $ ConnectionFailure err
         Right pip -> liftIO $ access pip mode database action

getMongoAccessMode :: (MonadIO m, MonadState app m, HasMongoDB app) => m AccessMode
getMongoAccessMode = mongoAccessMode `liftM` gets getMongoDB
{-# INLINE getMongoAccessMode #-}
