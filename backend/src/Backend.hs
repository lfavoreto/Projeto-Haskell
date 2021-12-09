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

getConn :: ConnectInfo
getConn = ConnectInfo "" --host
                      5432 --port
                      "" --user
                      "" --password
                      "" --database

migrationCont :: Query
migrationCont = "CREATE TABLE IF NOT EXISTS containers\
  \ (id SERIAL PRIMARY KEY, nome TEXT NOT NULL, tipo TEXT NOT NULL, status TEXT NOT NULL, categoria TEXT NOT NULL)"

migrationMov :: Query
migrationMov = "CREATE TABLE IF NOT EXISTS movimentacoes\
  \ (id SERIAL PRIMARY KEY, navio TEXT NOT NULL, movimentacao TEXT NOT NULL, dataInicio TEXT NOT NULL, dataFim TEXT NOT NULL)"

migrationDes :: Query
migrationDes = "CREATE TABLE IF NOT EXISTS destinos\
  \ (id SERIAL PRIMARY KEY, carga TEXT NOT NULL, bandeira TEXT NOT NULL, origem TEXT NOT NULL, destino TEXT NOT NULL)"

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

            BackendRoute_EditarMovimentacoes :/ mid -> method POST $ do
              movimentacoes <- A.decode <$> readRequestBody 2000
              case movimentacoes of
                Just mov -> do
                  liftIO $ do
                    execute_ dbcon migrationMov
                    execute dbcon "UPDATE movimentacoes SET navio = ?, \
                                  \ movimentacao = ?, dataInicio = ?, dataFim = ? WHERE id = ?"
                                  (navio mov, movimentacao mov, dataInicio mov, dataFim mov, mid)
                  modifyResponse $ setResponseStatus 200 "OK"
                Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"

            BackendRoute_ListarMovimentacoes :/ () -> method GET $ do
              res :: [Movimentacoes] <- liftIO $ do
                execute_ dbcon migrationMov
                query_ dbcon "SELECT * from movimentacoes"
              modifyResponse $ setResponseStatus 200 "OK"
              writeLazyText (encodeToLazyText res)

            BackendRoute_ApagarMovimentacoes :/ mid -> method POST $ do
              res :: [Movimentacoes] <- liftIO $ do
                execute_ dbcon migrationMov
                query dbcon "SELECT * from movimentacoes where id=?" (Only (mid :: Int))
              if res /= [] then do
                liftIO $ do
                  execute_ dbcon migrationMov
                  execute dbcon "DELETE from movimentacoes where id=?" (Only (mid :: Int))
                modifyResponse $ setResponseStatus 200 "OK"
              else
                modifyResponse $ setResponseStatus 404 "NOT FOUND"

            BackendRoute_BuscarMovimentacoes :/ mid -> method GET $ do
              res :: [Movimentacoes] <- liftIO $ do
                execute_ dbcon migrationMov
                query dbcon "SELECT * from movimentacoes where id=?" (Only (mid :: Int))
              if res /= [] then do
                modifyResponse $ setResponseStatus 200 "OK"
                writeLazyText (encodeToLazyText (Prelude.head res))
              else
                modifyResponse $ setResponseStatus 404 "NOT FOUND"

            BackendRoute_Movimentacoes :/ () -> method POST $ do
              movimentacoes <- A.decode <$> readRequestBody 2000
              case movimentacoes of
                Just mov -> do
                  liftIO $ do
                    execute_ dbcon migrationMov
                    execute dbcon 
                            "INSERT INTO movimentacoes (navio,movimentacao,dataInicio,dataFim) VALUES (?,?,?,?)" 
                            (navio mov, movimentacao mov, dataInicio mov, dataFim mov)
                  modifyResponse $ setResponseStatus 200 "OK"
                Nothing -> modifyResponse $ setResponseStatus 500 "Erro"

            BackendRoute_EditarDestinos :/ did -> method POST $ do
              destinos <- A.decode <$> readRequestBody 2000
              case destinos of
                Just dest -> do
                  liftIO $ do
                    execute_ dbcon migrationDes
                    execute dbcon "UPDATE destinos SET carga = ?, \
                                  \ bandeira = ?, origem = ?, destino = ? WHERE id = ?"
                                  (carga dest, bandeira dest, origem dest, destino dest, did)
                  modifyResponse $ setResponseStatus 200 "OK"
                Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
                
            BackendRoute_ListarDestinos :/ () -> method GET $ do
              res :: [Destinos] <- liftIO $ do
                execute_ dbcon migrationDes
                query_ dbcon "SELECT * from destinos"
              modifyResponse $ setResponseStatus 200 "OK"
              writeLazyText (encodeToLazyText res)

            BackendRoute_ApagarDestinos :/ did -> method POST $ do
              res :: [Destinos] <- liftIO $ do
                execute_ dbcon migrationDes
                query dbcon "SELECT * from destinos where id=?" (Only (did :: Int))
              if res /= [] then do
                liftIO $ do
                  execute_ dbcon migrationDes
                  execute dbcon "DELETE from destinos where id=?" (Only (did :: Int))
                modifyResponse $ setResponseStatus 200 "OK"
              else
                modifyResponse $ setResponseStatus 404 "NOT FOUND"

            BackendRoute_BuscarDestinos :/ did -> method GET $ do
              res :: [Destinos] <- liftIO $ do
                execute_ dbcon migrationDes
                query dbcon "SELECT * from destinos where id=?" (Only (did :: Int))
              if res /= [] then do
                modifyResponse $ setResponseStatus 200 "OK"
                writeLazyText (encodeToLazyText (Prelude.head res))
              else
                modifyResponse $ setResponseStatus 404 "NOT FOUND"

            BackendRoute_Destinos :/ () -> method POST $ do
              destinos <- A.decode <$> readRequestBody 2000
              case destinos of
                Just dest -> do
                  liftIO $ do
                    execute_ dbcon migrationDes
                    execute dbcon 
                            "INSERT INTO destinos (carga,bandeira,origem,destino) VALUES (?,?,?,?)" 
                            (carga dest, bandeira dest, origem dest, destino dest)
                  modifyResponse $ setResponseStatus 200 "OK"
                Nothing -> modifyResponse $ setResponseStatus 500 "Erro" 
            _ -> return ()
        return ()
  , _backend_routeEncoder = fullRouteEncoder
  }