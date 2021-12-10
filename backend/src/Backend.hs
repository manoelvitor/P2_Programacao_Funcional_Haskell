-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-- module Backend where

-- import Common.Route
-- import Common.Api

-- import Obelisk.Backend
-- import Database.PostgreSQL.Simple
-- import Data.Text
-- import Obelisk.Route
-- import Snap.Core
-- import Control.Monad.IO.Class (liftIO)
-- import qualified Data.Aeson as A
-- import Common.Api
-- import Data.Aeson.Text


-- getConn :: ConnectInfo
-- getConn = ConnectInfo "ec2-54-221-74-111.compute-1.amazonaws.com" --HOST
--                       5432 --PORT
--                       "pyphdvnfdgutex" --USER
--                       "a26a934a5ea4b1fc718f2295b54f71bc1b9c29711b3ffde2ba91ced6b7fb6b12" --PASSWORD
--                       "d444a6mtmia9ll"--DATABASE


-- migration :: Query
-- migration = "CREATE TABLE IF NOT EXISTS cliente\
--   \ (id SERIAL PRIMARY KEY, nome TEXT NOT NULL)"


-- -- migrationFunc :: Query
-- -- migrationFunc = "CREATE TABLE IF NOT EXISTS funcionario (id SERIAL PRIMARY KEY, nome TEXT NOT NULL, cargo TEXT NOT NULL, salario DOUBLE NOT NULL)"
  

-- -- backend :: Backend BackendRoute FrontendRoute
-- -- backend = Backend



-- --   { _backend_run = \serve -> do
-- --         dbcon <- connect getConn
-- --         serve $ \case 

-- --             BackendRoute_Funcionario :/ () -> method POST $ do
-- --                 func <- A.decode <$> readRequestBody 2000
-- --                 case func of
-- --                     Just funcionario -> do
-- --                         liftIO $ do
-- --                         execute_ dbcon migrationFunc
-- --                         execute dbcon "INSERT INTO funcionario (nome,cargo,salario) VALUES (?,?,?)" 
-- --                         (funcNome funcionario, funcCargo funcionario, funcSalario funcionario)
-- --                         modifyResponse $ setResponseStatus 200 "OK"
-- --                     Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"

-- --             BackendRoute_Cliente :/ () -> do
-- --                 Just nome <- A.decode <$> readRequestBody 2000
-- --                 liftIO $ do 
-- --                      execute_ dbcon migration
-- --                      execute dbcon "INSERT INTO cliente (nome) VALUES (?)" [nome :: Text]
-- --                 modifyResponse $ setResponseStatus 200 "OK"
-- --             _ -> return ()
-- --   , _backend_routeEncoder = fullRouteEncoder
-- --   }

-- migrateProd :: Query
-- migrateProd = "CREATE TABLE IF NOT EXISTS produtoo (id SERIAL PRIMARY KEY, nome TEXT NOT NULL, valor REAL NOT NULL, qt INTEGER NOT NULL)"
 
-- backend :: Backend BackendRoute FrontendRoute
-- backend = Backend
--   { _backend_run = \serve -> do
--       dbcon <- connect getConn
--       serve $ do
--           \case 
--                 BackendRoute_Listar :/ () -> method GET $ do
--                     res :: [Produto] <- liftIO $ do
--                         execute_ dbcon migrateProd
--                         query_ dbcon "SELECT * from produtoo" 
--                     modifyResponse $ setResponseStatus 200 "OK"
--                     writeLazyText (encodeToLazyText res)
--                 BackendRoute_Buscar :/ pid -> method GET $ do
--                     res :: [Produto] <- liftIO $ do
--                         execute_ dbcon migrateProd
--                         query dbcon "SELECT * from produtoo WHERE id=?" (Only (pid :: Int))
--                     if res /= [] then do
--                         modifyResponse $ setResponseStatus 200 "OK"
--                         writeLazyText (encodeToLazyText (Prelude.head res))
--                     else
--                         modifyResponse $ setResponseStatus 404 "NOT FOUND"
--                 BackendRoute_Produto :/ () -> method POST $ do
--                     prod <- A.decode <$> readRequestBody 2000
--                     case prod of
--                          Just produto -> do
--                              liftIO $ do
--                                  execute_ dbcon migrateProd
--                                  execute dbcon "INSERT INTO produtoo (nome,valor,qt) VALUES (?,?,?)" 
--                                          (produtoNome produto, produtoValor produto, produtoQt produto)
--                              modifyResponse $ setResponseStatus 200 "OK"
--                          Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
--                 BackendRoute_Cliente :/ () -> method POST $ do
--                     Just nome <- A.decode <$> readRequestBody 2000
--                     liftIO $ do
--                         execute_ dbcon migrate
--                         execute dbcon "INSERT INTO cliente (nome) VALUES (?)" [nome :: Text]
--                     modifyResponse $ setResponseStatus 200 "OK"
--                 _ -> return ()
--   , _backend_routeEncoder = fullRouteEncoder
--   }



{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase, GADTs #-}

module Backend where

import Common.Route
import Obelisk.Backend
import Database.PostgreSQL.Simple
import Data.Text
import Obelisk.Route
import Snap.Core
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import Common.Api
import Data.Aeson.Text

getConn :: ConnectInfo
getConn  = ConnectInfo "ec2-54-221-74-111.compute-1.amazonaws.com"
                       5432
                       "pyphdvnfdgutex"
                       "a26a934a5ea4b1fc718f2295b54f71bc1b9c29711b3ffde2ba91ced6b7fb6b12"
                       "d444a6mtmia9ll"

migrate :: Query
migrate = "CREATE TABLE IF NOT EXISTS cliente (id SERIAL PRIMARY KEY, nome TEXT NOT NULL)"
 
migrateProd :: Query
migrateProd = "CREATE TABLE IF NOT EXISTS produtoo (id SERIAL PRIMARY KEY, nome TEXT NOT NULL, valor REAL NOT NULL, qt INTEGER NOT NULL)"
 

 
migrateFunc :: Query
migrateFunc = "CREATE TABLE IF NOT EXISTS funcionario (id SERIAL PRIMARY KEY, nome TEXT NOT NULL, salario REAL NOT NULL, cargo TEXT NOT NULL)"

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      dbcon <- connect getConn
      serve $ do
          \case 
                BackendRoute_Buscar :/ pid -> method GET $ do 
                    res :: [Funcionario] <- liftIO $ do
                            execute_ dbcon migrateFunc
                            query dbcon "SELECT * from funcionario where id=?" (Only (pid :: Int))
                    if res /= [] then do
                            modifyResponse $ setResponseStatus 200 "OK"   
                            writeLazyText (encodeToLazyText (Prelude.head res))
                    else 
                            modifyResponse $ setResponseStatus 404 "NOT FOUND"  
                BackendRoute_Editar :/ pid -> method POST $ do
                    fun <- A.decode <$> readRequestBody 2000
                    case fun of
                        Just funcionario -> do
                            liftIO $ do
                                execute_ dbcon migrateFunc
                                execute dbcon "UPDATE funcionario SET nome = ?, salario = ?, cargo = ? WHERE id = ?" 
                                        (nome funcionario,salario funcionario,cargo funcionario,pid)
                            modifyResponse $ setResponseStatus 200 "OK"
                        Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"          
                BackendRoute_Listar :/ () -> method GET $ do
                    res :: [Funcionario] <- liftIO $ do
                        execute_ dbcon migrateProd
                        query_ dbcon "SELECT * from funcionario" 
                    modifyResponse $ setResponseStatus 200 "OK"
                    writeLazyText (encodeToLazyText res)
                BackendRoute_Buscar :/ pid -> method GET $ do
                    res :: [Funcionario] <- liftIO $ do
                        execute_ dbcon migrateProd
                        query dbcon "SELECT * from funcionario WHERE id=?" (Only (pid :: Int))
                    if res /= [] then do
                        modifyResponse $ setResponseStatus 200 "OK"
                        writeLazyText (encodeToLazyText (Prelude.head res))
                    else
                        modifyResponse $ setResponseStatus 404 "NOT FOUND"
                BackendRoute_Funcionario :/ () -> method POST $ do
                    fun <- A.decode <$> readRequestBody 4000
                    case fun of
                         Just funcionario -> do
                             liftIO $ do
                                 execute_ dbcon migrateFunc
                                 execute dbcon "INSERT INTO funcionario (nome,salario,cargo) VALUES (?,?,?)" 
                                         (nome funcionario, salario funcionario, cargo funcionario)
                             modifyResponse $ setResponseStatus 200 "OK"
                         Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
                BackendRoute_Cliente :/ () -> method POST $ do
                    Just nome <- A.decode <$> readRequestBody 2000
                    liftIO $ do
                        execute_ dbcon migrate
                        execute dbcon "INSERT INTO cliente (nome) VALUES (?)" [nome :: Text]
                    modifyResponse $ setResponseStatus 200 "OK"                  
                _ -> return ()
  , _backend_routeEncoder = fullRouteEncoder
  }

