{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Common.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import Data.Text (Text, unpack)
import Data.Function
import Data.Functor.Identity

import Obelisk.Route
import Obelisk.Route.TH

data BackendRoute :: * -> * where
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Containers :: BackendRoute ()
  BackendRoute_ListarContainers :: BackendRoute ()
  BackendRoute_BuscarContainers :: BackendRoute Int
  BackendRoute_ApagarContainers :: BackendRoute Int
  BackendRoute_EditarContainers :: BackendRoute Int

  BackendRoute_Movimentacoes :: BackendRoute ()
  BackendRoute_ListarMovimentacoes :: BackendRoute ()
  BackendRoute_BuscarMovimentacoes :: BackendRoute Int
  BackendRoute_EditarMovimentacoes :: BackendRoute Int
  BackendRoute_ApagarMovimentacoes :: BackendRoute Int

  BackendRoute_Destinos :: BackendRoute ()
  BackendRoute_ListarDestinos :: BackendRoute ()
  BackendRoute_BuscarDestinos :: BackendRoute Int
  BackendRoute_EditarDestinos :: BackendRoute Int
  BackendRoute_ApagarDestinos :: BackendRoute Int

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

checkedFullRouteEncoder
  :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
checkedFullRouteEncoder = checkEncoder fullRouteEncoder & \case
  Left err -> error $ unpack err
  Right encoder -> encoder

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_Containers -> PathSegment "containers" $ unitEncoder mempty
      BackendRoute_ListarContainers -> PathSegment "listar-containers" $ unitEncoder mempty
      BackendRoute_BuscarContainers -> PathSegment "buscar-containers" readShowEncoder
      BackendRoute_ApagarContainers -> PathSegment "apagar-containers" readShowEncoder
      BackendRoute_EditarContainers -> PathSegment "editar-containers" readShowEncoder

      BackendRoute_Movimentacoes -> PathSegment "movimentacoes" $ unitEncoder mempty
      BackendRoute_ListarMovimentacoes -> PathSegment "listar-movimentacoes" $ unitEncoder mempty
      BackendRoute_BuscarMovimentacoes -> PathSegment "buscar-movimentacoes" readShowEncoder
      BackendRoute_ApagarMovimentacoes -> PathSegment "apagar-movimentacoes" readShowEncoder
      BackendRoute_EditarMovimentacoes -> PathSegment "editar-movimentacoes" readShowEncoder

      BackendRoute_Destinos -> PathSegment "destinos" $ unitEncoder mempty
      BackendRoute_ListarDestinos -> PathSegment "listar-destinos" $ unitEncoder mempty
      BackendRoute_BuscarDestinos -> PathSegment "buscar-destinos" readShowEncoder
      BackendRoute_ApagarDestinos -> PathSegment "apagar-destinos" readShowEncoder
      BackendRoute_EditarDestinos -> PathSegment "editar-destinos" readShowEncoder)
  (\case
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty)

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
