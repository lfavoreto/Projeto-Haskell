{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)
import Data.Maybe
import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static
import Text.Read (readMaybe)
import Reflex.Dom.Core

import Common.Api
import Common.Route


getPath :: T.Text
getPath = renderBackendRoute checkedFullRouteEncoder $ BackendRoute_Container :/ ()

nomeRequest :: T.Text -> XhrRequest T.Text
nomeRequest s = postJson getPath (Container s)

pagReq :: ( DomBuilder t m
          , Prerender js t m
          ) => m (Event t T.Text)
pagReq = do
    inpnome <- inputElement def
    (submitBtn,_) <- el' "button" (text "Inserir")
    let click = domEvent Click submitBtn
    let nm = tag (current $ _inputElement_value inpnome) click
    st <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (nomeRequest <$> nm))
    return (fromMaybe "" <$> switchDyn st)

paginaInsere :: ( DomBuilder t m
                , PostBuild t m 
                , MonadHold t m
                , Prerender js t m
                ) => m ()
paginaInsere = do
    el "main" $ do
      elAttr "div" ("class" =: "container") $ do
        el "h2" $ text "Adicionar Container"
        elAttr "label" ("class" =: "inputLabel") $ text "Nome:"
        st <- pagReq
        tx <- holdDyn "" st
        el "div" (dynText tx)


home :: DomBuilder t m => m ()
home = do
  elAttr "main" ("class" =: "center") $ do
    elAttr "div" ("class" =: "container") $ do
      el "h2" $ text "Containers"
      elAttr "img" ("src" =: static @"images.jpeg") blank
      el "h3" $ text "Página em Construção"

pagina2 :: DomBuilder t m => m ()
pagina2 = do
  elAttr "main" ("class" =: "center") $ do
    elAttr "div" ("class" =: "container") $ do
      el "h2" $ text "Movimentações"
      elAttr "img" ("src" =: static @"images.jpeg") blank
      el "h3" $ text "Página em Construção"

pagina3 :: DomBuilder t m => m ()
pagina3 = do
  elAttr "main" ("class" =: "center") $ do
    elAttr "div" ("class" =: "container") $ do
      el "h2" $ text "Navios"
      elAttr "img" ("src" =: static @"images.jpeg") blank
      el "h3" $ text "Página em Construção"

data Pagina = Pagina1 | Pagina2 | Pagina3 | Pagina4

clickLi :: DomBuilder t m => Pagina -> T.Text -> m (Event t Pagina)
clickLi p t = do
  (ev, _) <- el' "li" (elAttr "a" ("href" =: "#") (text t))
  return ((\_ -> p) <$> domEvent Click ev)

menuLi :: (DomBuilder t m, MonadHold t m) => m (Dynamic t Pagina)
menuLi = do
  evs <- elAttr "ul" ("class" =: "d-flex") $ do
    p1 <- clickLi Pagina1 "Containers"
    p2 <- clickLi Pagina2 "Movimentações"
    p3 <- clickLi Pagina3 "Navios"
    p4 <- clickLi Pagina4 "Adicionar Container"
    return (leftmost [p1, p2, p3, p4])
  holdDyn Pagina1 evs

currPag :: (DomBuilder t m, MonadHold t m, PostBuild t m, Prerender js0 t m) 
        => Pagina -> m ()
currPag p = 
    case p of
          Pagina1 -> home
          Pagina2 -> pagina2
          Pagina3 -> pagina3
          Pagina4 -> paginaInsere

mainPag :: (DomBuilder t m, MonadHold t m, PostBuild t m, Prerender js0 t m) => m ()
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
