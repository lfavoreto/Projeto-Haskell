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
migration = "CREATE TABLE IF NOT EXISTS container\
  \ (id SERIAL PRIMARY KEY, nome TEXT NOT NULL)"

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
        dbcon <- connect getConn
        serve $ \case 
            BackendRoute_Container :/ () -> do
                Just nome <- A.decode <$> readRequestBody 2000
                liftIO $ do 
                     execute_ dbcon migration
                     execute dbcon "INSERT INTO container (nome) VALUES (?)" [nome :: Text]
                modifyResponse $ setResponseStatus 200 "OK"
            _ -> return ()
        return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
