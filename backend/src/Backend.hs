{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Backend where

import Common.Route
import Obelisk.Backend
import Obelisk.Route
import Database.PostgreSQL.Simple 
import Snap.Core
import qualified Data.Aeson as A
import Data.Text
import Control.Monad.IO.Class (liftIO)

migration :: Query
migration = "CREATE TABLE IF NOT EXISTS cliente\
  \ (id SERIAL PRIMARY KEY, nome TEXT NOT NULL)"

getConn :: ConnectInfo
getConn = ConnectInfo "" --HOST
                      5432 --PORT
                      "" --USER
                      "" --PASSWORD
                      ""--DATABASE

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
        dbcon <- connect getConn
        serve $ \case 
            BackendRoute_Cliente :/ () -> do
                Just nome <- A.decode <$> readRequestBody 2000
                liftIO $ do 
                     execute_ dbcon migration
                     execute dbcon "INSERT INTO cliente (nome) VALUES (?)" [nome :: Text]
                modifyResponse $ setResponseStatus 200 "OK"
            _ -> return ()
        return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
