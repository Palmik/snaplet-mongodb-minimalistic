{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ExtendedDefaultRules      #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Example.Foo
( makeTeamDocument
, documentsSplice
, module Database.MongoDB
) where

import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.Encoding as T (decodeUtf8)

import Snap
import Snap.Snaplet
import Snap.Snaplet.MongoDB

import Text.Templating.Heist

import Database.MongoDB
import Control.Monad.Trans (liftIO)
    
makeTeamDocument name city = ["name" =: name, "city" =: city]

documentsSplice :: (HasMongoDB m) => Collection -> Splice (Handler m m)
documentsSplice collection = do
    eres <- eitherWithDB $ rest =<< find (select [] collection)
    res  <- return $ either (const []) id eres
    mapSplices (runChildrenWithText . showAs "document") res

showAs :: (Show a) => Text -> a -> [(Text, Text)]
showAs name x = [(name, T.pack $ show x)]