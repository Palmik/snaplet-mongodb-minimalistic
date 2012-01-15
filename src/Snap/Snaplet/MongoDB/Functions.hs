{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Snap.Snaplet.MongoDB.Functions
( eitherWithDB'
, eitherWithDB
, maybeWithDB
, maybeWithDB'
, unsafeWithDB
, unsafeWithDB'
) where

import            Control.Monad.Error

import            Snap
import            Snap.Snaplet.MongoDB.Core

import            Database.MongoDB
import            System.IO.Pool

class (MonadIO m, MonadState app m, HasMongoDB app) => HasMongoDB' app m
instance (MonadIO m, MonadState app m, HasMongoDB app) => HasMongoDB' app m

------------------------------------------------------------------------------
-- |
unsafeWithDB :: (HasMongoDB' app m) => Action IO a -> m a
unsafeWithDB = unsafeWithDB' UnconfirmedWrites

------------------------------------------------------------------------------
-- |
unsafeWithDB' :: (HasMongoDB' app m) => AccessMode -> Action IO a -> m a
unsafeWithDB' mode action = do
    res <- (eitherWithDB' mode action)
    return $ either (error . show) id res

------------------------------------------------------------------------------
-- |
maybeWithDB :: (HasMongoDB' app m) => Action IO a -> m (Maybe a)
maybeWithDB = maybeWithDB' UnconfirmedWrites

------------------------------------------------------------------------------
-- |
maybeWithDB' :: (HasMongoDB' app m) => AccessMode -> Action IO a -> m (Maybe a)
maybeWithDB' mode action = do
    res <- (eitherWithDB' mode action)
    return $ either (const Nothing) Just res

------------------------------------------------------------------------------
-- |
eitherWithDB :: (HasMongoDB' app m) => Action IO a -> m (Either Failure a)
eitherWithDB = eitherWithDB' UnconfirmedWrites

------------------------------------------------------------------------------
-- |
eitherWithDB' :: (HasMongoDB' app m) => AccessMode -> Action IO a -> m (Either Failure a)
eitherWithDB' mode action = do
    (MongoDB pool database) <- gets getMongoDB
    ep <- liftIO $ runErrorT $ aResource pool
    case ep of
         Left  err -> return $ Left $ ConnectionFailure err
         Right pip -> liftIO $ access pip mode database action