module Snap.Snaplet.MongoDB.Core
( MongoDB(..)
, HasMongoDB(..)
) where

import            Database.MongoDB
import            System.IO.Pool

------------------------------------------------------------------------------
-- | Snaplet's data type.
--
-- Example:
-- @
-- data App = App
--     { _heist :: Snaplet (Heist App)
--     , _database :: Snaplet MongoDB
--     }
-- @
data MongoDB = MongoDB
    { mongoPool     :: Pool IOError Pipe
    , mongoDatabase :: Database
    }

------------------------------------------------------------------------------
-- | Snaplet's type-class.
--
-- Example:
-- @
-- instance HasMongoDB App where
--     getMongoDB = getL (snapletValue . database)
-- @
class HasMongoDB app where
    getMongoDB :: app -> MongoDB
    