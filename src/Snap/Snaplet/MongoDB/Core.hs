module Snap.Snaplet.MongoDB.Core
( MongoDB(..)
, HasMongoDB(..)
) where

import            Database.MongoDB
import            System.IO.Pool

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
    