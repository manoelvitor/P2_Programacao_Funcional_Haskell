-- {-# LANGUAGE DeriveGeneric #-}
-- {-# language DeriveAnyClass  #-}
-- {-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- module Common.Api where

-- import Data.Aeson
-- import Data.Text (Text)
-- import GHC.Generics (Generic)
-- import Database.PostgreSQL.Simple



-- data Cliente = Cliente Text deriving (Generic, ToJSON, FromJSON)

-- -- data Funcionario = Funcionario {
-- --     funcId :: Int,
-- --     funcNome :: Text,
-- --     funcCargo :: Text,
-- --     funcSalario :: Double
-- -- } deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)
{-# LANGUAGE DeriveGeneric #-}
{-# language DeriveAnyClass  #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Common.Api where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple

-- As definiçoes de tabela vao aqui.
data Cliente = Cliente Text deriving (Generic, ToJSON, FromJSON)

data Produto = Produto {
    produtoId :: Int,
    produtoNome :: Text,
    produtoValor :: Double,
    produtoQt :: Int
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)


data Funcionario = Funcionario {
    funcionarioId :: Int,
    nome :: Text,
    salario :: Double,
    cargo :: Text
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)