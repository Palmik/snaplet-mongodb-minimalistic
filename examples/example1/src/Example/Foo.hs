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

import Snap
import Snap.Snaplet.MongoDB

import Heist.Interpreted

import Database.MongoDB
    
makeTeamDocument name city = ["name" =: name, "city" =: city]

documentsSplice :: (HasMongoDB m) => Collection -> Splice (Handler m m)
documentsSplice collection = do
    eres <- eitherWithDB $ rest =<< find (select [] collection)
    mapSplices (runChildrenWithText . showAs "document")
               (either (const []) id eres)

showAs :: (Show a) => Text -> a -> [(Text, Text)]
showAs name x = [(name, T.pack $ show x)]
