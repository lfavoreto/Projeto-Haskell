{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
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

data Containers = Containers {
    idContainer :: Int,
    nomeContainer :: Text,
    tipoContainer :: Text,
    statusContainer :: Text,
    categoriaContainer :: Text
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

data Movimentacoes = Movimentacoes {
    idMovimentacao :: Int,
    navio :: Text,
    movimentacao :: Text,
    dataInicio :: Text,
    dataFim :: Text
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)