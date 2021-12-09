{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Backend where

import Common.Route
import Obelisk.Backend
import Obelisk.Route
import Database.PostgreSQL.Simple 
import Snap.Core
import qualified Data.Aeson as A
import Control.Monad.IO.Class (liftIO)
import Common.Api
import Data.Aeson.Text


migrationCont :: Query
migrationCont = "CREATE TABLE IF NOT EXISTS containers\
  \ (id SERIAL PRIMARY KEY, nome TEXT NOT NULL, tipo TEXT NOT NULL, status TEXT NOT NULL, categoria TEXT NOT NULL)"

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
        dbcon <- connect getConn
        serve $ \case 
            BackendRoute_EditarContainers :/ cid -> method POST $ do
              containers <- A.decode <$> readRequestBody 2000
              case containers of
                Just cont -> do
                  liftIO $ do
                    execute_ dbcon migrationCont
                    execute dbcon "UPDATE containers SET nome = ?, \
                                  \ tipo = ?, status = ?, categoria = ? WHERE id = ?"
                                  (nomeContainer cont, tipoContainer cont, statusContainer cont, categoriaContainer cont, cid)
                  modifyResponse $ setResponseStatus 200 "OK"
                Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
                
            BackendRoute_ListarContainers :/ () -> method GET $ do
              res :: [Containers] <- liftIO $ do
                execute_ dbcon migrationCont
                query_ dbcon "SELECT * from containers"
              modifyResponse $ setResponseStatus 200 "OK"
              writeLazyText (encodeToLazyText res)

            BackendRoute_ApagarContainers :/ cid -> method POST $ do
              res :: [Containers] <- liftIO $ do
                execute_ dbcon migrationCont
                query dbcon "SELECT * from containers where id=?" (Only (cid :: Int))
              if res /= [] then do
                liftIO $ do
                  execute_ dbcon migrationCont
                  execute dbcon "DELETE from containers where id=?" (Only (cid :: Int))
                modifyResponse $ setResponseStatus 200 "OK"
              else
                modifyResponse $ setResponseStatus 404 "NOT FOUND"

            BackendRoute_BuscarContainers :/ cid -> method GET $ do
              res :: [Containers] <- liftIO $ do
                execute_ dbcon migrationCont
                query dbcon "SELECT * from containers where id=?" (Only (cid :: Int))
              if res /= [] then do
                modifyResponse $ setResponseStatus 200 "OK"
                writeLazyText (encodeToLazyText (Prelude.head res))
              else
                modifyResponse $ setResponseStatus 404 "NOT FOUND"

            BackendRoute_Containers :/ () -> method POST $ do
              containers <- A.decode <$> readRequestBody 2000
              case containers of
                Just cont -> do
                  liftIO $ do
                    execute_ dbcon migrationCont
                    execute dbcon 
                            "INSERT INTO containers (nome,tipo,status,categoria) VALUES (?,?,?,?)" 
                            (nomeContainer cont, tipoContainer cont, statusContainer cont, categoriaContainer cont)
                  modifyResponse $ setResponseStatus 200 "OK"
                Nothing -> modifyResponse $ setResponseStatus 500 "Erro"
            _ -> return ()
        return ()
  , _backend_routeEncoder = fullRouteEncoder
  }