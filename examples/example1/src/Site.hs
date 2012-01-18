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



