{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route

home :: DomBuilder t m => m ()
home = do
  el "main" $ do
    elAttr "div" ("class" =: "container") $ do
      el "h2" $ text "Home"
      elAttr "img" ("src" =: static @"images.jpeg") blank
      el "h3" $ text "Página em Construção"

pagina2 :: DomBuilder t m => m ()
pagina2 = do
  el "main" $ do
    elAttr "div" ("class" =: "container") $ do
      el "h2" $ text "Sobre"
      elAttr "img" ("src" =: static @"images.jpeg") blank
      el "h3" $ text "Página em Construção"

pagina3 :: DomBuilder t m => m ()
pagina3 = do
  el "main" $ do
    elAttr "div" ("class" =: "container") $ do
      el "h2" $ text "Contato"
      elAttr "img" ("src" =: static @"images.jpeg") blank
      el "h3" $ text "Página em Construção"

data Pagina = Pagina1 | Pagina2 | Pagina3

clickLi :: DomBuilder t m => Pagina -> T.Text -> m (Event t Pagina)
clickLi p t = do
  (ev, _) <- el' "li" (elAttr "a" ("href" =: "#") (text t))
  return ((\_ -> p) <$> domEvent Click ev)

menuLi :: (DomBuilder t m, MonadHold t m) => m (Dynamic t Pagina)
menuLi = do
  evs <- elAttr "ul" ("class" =: "d-flex") $ do
    p1 <- clickLi Pagina1 "Home"
    p2 <- clickLi Pagina2 "Página 2"
    p3 <- clickLi Pagina3 "Página 3"
    return (leftmost [p1, p2, p3])
  holdDyn Pagina1 evs

currPag :: (DomBuilder t m, MonadHold t m, PostBuild t m) 
        => Pagina -> m ()
currPag p = 
    case p of
          Pagina1 -> home
          Pagina2 -> pagina2
          Pagina3 -> pagina3

mainPag :: (DomBuilder t m, MonadHold t m, PostBuild t m) => m ()
mainPag = do
  pag <- el "header" $ do
    elAttr "div" ("class" =: "container d-flex align-items-center justify-content-between pt-5 pb-5") $ do
      el "h1" $ text "Projeto Haskell - SI"
      menuLi
  dyn_ $ currPag <$> pag

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Projeto Haskell"
      elAttr "meta" ("charset" =: "UTF-8") blank 
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "link" ("href" =: static @"bootstrap.min.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      mainPag
      
      return ()
  }
