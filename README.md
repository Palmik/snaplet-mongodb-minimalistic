# About

`snaplet-mongodb-minimalistic' is minimal implementation of Snaplet for MongoDB.

The package follows the [Snaplet Design](http://snapframework.com/docs/tutorials/snaplets-design).

The package itself is divided into two parts:

  1. `Snap.Snaplet.MongoDB.Core'
  2. `Snap.Snaplet.MongoDB.Functions'

The `Core' package contains the Snaplet's data type (`MongoDB') and typeclass (`HasMongoDB').

The `Functions' package contains the basic functions (initializers and functions for querying the database).

# Examples

## Example #1

We will follow the common Snap project structure.

### src/Application.hs

    {-# LANGUAGE TemplateHaskell       #-}
    {-# LANGUAGE MultiParamTypeClasses #-}
    {-# LANGUAGE TypeSynonymInstances  #-}

    module Application where

    import Data.Lens.Template
    import Data.Lens.Common

    import Snap.Snaplet
    import Snap.Snaplet.Heist
    import Snap.Snaplet.MongoDB.Core

    import Control.Category
    import Prelude hiding ((.))

    data App = App
        { _heist :: Snaplet (Heist App)
        , _database :: Snaplet MongoDB
        }

    type AppHandler = Handler App App

    makeLens ''App

    instance HasHeist App where
        heistLens = subSnaplet heist

    instance HasMongoDB App where
        getMongoDB = getL (snapletValue . database)

### src/Example/Foo.hs

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

### src/Site.hs

    {-# LANGUAGE OverloadedStrings #-}

    module Site
    ( app
    ) where

    import           Data.ByteString (ByteString)
    import           Data.ByteString.UTF8 (toString)

    import           Snap.Core
    import           Snap.Util.FileServe

    import           Snap.Snaplet
    import           Snap.Snaplet.Heist
    import           Snap.Snaplet.MongoDB

    import           Text.Templating.Heist
    import           Text.XmlHtml hiding (render)

    import           Application
    import           Example.Foo

    indexView :: Handler App App ()
    indexView = ifTop $ heistLocal (bindSplices indexSplices) $ render "index"
      where
        indexSplices =
            [ ("documents", documentsSplice "test-collection")
            ]

    indexHandler :: Handler App App ()
    indexHandler = insertTeamHandler >> redirect "/"

    insertTeamHandler :: Handler App App ()
    insertTeamHandler = do
        name <- getParamOr "form1-name" (redirect "/")
        city <- getParamOr "form1-city" (redirect "/")
        eitherWithDB $ insert "test-collection" $ makeTeamDocument name city
        return ()
        where getParamOr param action = getParam param >>= maybe action (return . toString)

    routes :: [(ByteString, Handler App App ())]
    routes = [ ("/", method POST indexHandler)
             , ("/",             indexView)
             , ("",  with heist heistServe)
             , ("",  serveDirectory "resources/static")
             ]

    app :: SnapletInit App App
    app = makeSnaplet "app" "An snaplet example application." Nothing $ do
        h <- nestSnaplet "heist" heist $ heistInit "resources/templates"
        d <- nestSnaplet "database" database $ mongoDBInit 10 (host "127.0.0.1") "Snaplet-MongoDB"
        addRoutes routes
        return $ App h d

    And that's it.
