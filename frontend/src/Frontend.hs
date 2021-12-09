{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}

module Frontend where

import Control.Monad
import Control.Monad.Fix
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
import Data.Aeson (ToJSON)
import Common.Api
import Common.Route

getPath :: R BackendRoute -> T.Text
getPath p = renderBackendRoute checkedFullRouteEncoder p

sendRequest :: ToJSON a => R BackendRoute -> a -> XhrRequest T.Text
sendRequest r dados = postJson (getPath r) dados

reqCont :: ( DomBuilder t m
      , Prerender js t m
      ) => m ()
reqCont = do
  elAttr "div" ("class" =: "container") $ do
    el "h2" $ text "Inserir Container"
    elAttr "div" ("class" =: "form-group") $ do
      elAttr "p" ("class" =: "label") $ text "Nome:"
      nome <- inputElement def
      elAttr "p" ("class" =: "label") $ text "Tipo:"
      tipo <- inputElement def
      elAttr "p" ("class" =: "label") $ text "Status:"
      status <- inputElement def
      elAttr "p" ("class" =: "label") $ text "Categoria"
      categoria <- inputElement def
      let cont = fmap (\((n,t),(s,c)) -> Containers 0 n t s c) (zipDyn (zipDyn (_inputElement_value nome) (_inputElement_value tipo)) (zipDyn (_inputElement_value status) (_inputElement_value categoria)))
      (submitBtn,_) <- el' "button" (text "Inserir")
      let click = domEvent Click submitBtn
      let contEvt = tag (current cont) click
      _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
          (pure never)
          (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_Containers :/ ()) <$> contEvt))
      return ()

getListReq :: XhrRequest ()
getListReq = xhrRequest "GET" (getPath (BackendRoute_ListarContainers :/ ())) def

data Acao = Perfil Int | Editar Int | Apagar Int

tabRegistro :: (PostBuild t m, DomBuilder t m) => Dynamic t Containers -> m (Event t Acao)
tabRegistro pr = do
  el "tr" $ do
    el "td" (dynText $ fmap (T.pack . show . idContainer) pr)
    el "td" (dynText $ fmap (T.pack . show . nomeContainer) pr)
    el "td" (dynText $ fmap (T.pack . show . tipoContainer) pr)
    el "td" (dynText $ fmap (T.pack . show . statusContainer) pr)
    el "td" (dynText $ fmap (T.pack . show . categoriaContainer) pr)
    evt <- fmap (fmap (const Perfil)) (elAttr "td" ("class" =: "ver") $ do button "Ver")
    evt2 <- fmap (fmap (const Editar)) (elAttr "td" ("class" =: "editar") $ do button "Editar")
    evt3 <- fmap (fmap (const Apagar)) (elAttr "td" ("class" =: "apagar") $ do button "Apagar")
    return (attachPromptlyDynWith (flip ($)) (fmap idContainer pr) (leftmost [evt,evt2,evt3]))

reqTabela :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Workflow t m T.Text
reqTabela = Workflow $ do
  elAttr "div" ("class" =: "container") $ do
    el "h2" $ text "Containers" 
    btn <- button "Listar"
    conts :: Dynamic t (Event t (Maybe [Containers])) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (const getListReq <$> btn))
    evt <- return (fmap (fromMaybe []) $ switchDyn conts)
    dynP <- foldDyn (++) [] evt
    tb <- elAttr "table" ("class" =: "table table-striped") $ do
      elAttr "thead" ("class" =: "thead-dark") $ do
        el "tr" $ do
          elAttr "th" ("scope" =: "col") (text "Id")
          elAttr "th" ("scope" =: "col") (text "Nome")
          elAttr "th" ("scope" =: "col") (text "Tipo")
          elAttr "th" ("scope" =: "col") (text "Status")
          elAttr "th" ("scope" =: "col") (text "Categoria")
          elAttr "th" ("scope" =: "col") blank
          elAttr "th" ("scope" =: "col") blank
          elAttr "th" ("scope" =: "col") blank
      
      el "tbody" $ do
        simpleList dynP tabRegistro
    tb' <- return $ switchDyn $ fmap leftmost tb
    return ("", escolherPag <$> tb')
    where
      escolherPag (Perfil cid) = pagPerfil cid
      escolherPag (Editar cid) = editarPerfil cid
      escolherPag (Apagar cid) = apagarPerfil cid

getContReq :: Int -> XhrRequest ()
getContReq cid = xhrRequest "GET" (getPath (BackendRoute_BuscarContainers :/ cid)) def

pagPerfil :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
pagPerfil cid = Workflow $ do
  elAttr "div" ("class" =: "container div-ver") $ do
    el "h2" $ text "Info"
    elAttr "div" ("class" =: "id") $ do
      el "span" $ text "Container ID: "
      el "span" $ text (T.pack $ show cid) 
    btn <- button "Ver"
    cont :: Dynamic t (Event t (Maybe Containers)) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (const (getContReq cid) <$> btn))
    mdyn <- holdDyn Nothing (switchDyn cont)
    dynP <- return ((fromMaybe (Containers 0 "" "" "" "")) <$> mdyn)
    ret <- button "Voltar"
    elAttr "div" ("class" =: "dados") $ do
      elAttr "div" ("class" =: "campo") $ do
        el "span" (text "Nome: ")
        el "span" (dynText $ fmap nomeContainer dynP)
      elAttr "div" ("class" =: "campo") $ do
        el "span" (text "Tipo: ")
        el "span" (dynText $ fmap tipoContainer dynP)
      elAttr "div" ("class" =: "campo") $ do
        el "span" (text "Status: ")
        el "span" (dynText $ fmap statusContainer dynP)
      elAttr "div" ("class" =: "campo") $ do
        el "span" (text "Categoria: ")
        el "span" (dynText $ fmap categoriaContainer dynP)
    return ("" <> "", reqTabela <$ ret)

editarPerfil :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
editarPerfil cid = Workflow $ do
  elAttr "div" ("class" =: "container div-ver") $ do
    el "h2" $ text "Editar"
    elAttr "div" ("class" =: "id") $ do
      el "span" $ text "Container ID: "
      el "span" $ text (T.pack $ show cid) 
    btn <- button "Mostrar"
    submitBtn <- button "Salvar"
    elAttr "div" ("class" =: "form-group") $ do
      cont :: Dynamic t (Event t (Maybe Containers)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const (getContReq cid) <$> btn))
      mdyn <- return (switchDyn cont)
      dynE <- return ((fromMaybe (Containers 0 "" "" "" "")) <$> mdyn)

      elAttr "p" ("class" =: "label") $ text "Nome"
      nome <- inputElement $ def & inputElementConfig_setValue .~ (fmap nomeContainer dynE)
      elAttr "p" ("class" =: "label") $ text "Tipo"
      tipo <- inputElement $ def & inputElementConfig_setValue .~ (fmap tipoContainer dynE)
      elAttr "p" ("class" =: "label") $ text "Status"
      status <- inputElement $ def & inputElementConfig_setValue .~ (fmap statusContainer dynE)
      elAttr "p" ("class" =: "label") $ text "Categoria"
      categoria <- inputElement $ def & inputElementConfig_setValue .~ (fmap categoriaContainer dynE)

      let cont = fmap (\((n,t),(s,c)) -> Containers 0 n t s c) (zipDyn (zipDyn (_inputElement_value nome) (_inputElement_value tipo)) (zipDyn (_inputElement_value status) (_inputElement_value categoria)))
      let contEvt = tag (current cont) submitBtn
      _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
          (pure never)
          (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_EditarContainers :/ cid) <$> contEvt))
      return ("" <> "", reqTabela <$ submitBtn)

apagarPerfil :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
apagarPerfil cid = Workflow $ do
  elAttr "div" ("class" =: "container div-ver") $ do
    el "h2" $ text "Apagar?"
    elAttr "div" ("class" =: "id") $ do
      el "span" $ text "Container ID: "
      el "span" $ text (T.pack $ show cid)
    btn <- button "Apagar"
    ret <- button "Voltar"
    cont :: Dynamic t (Event t (Maybe T.Text)) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_ApagarContainers :/ cid) <$> btn))
    return ("" <> "", reqTabela <$ ret)

reqListaCont :: ( DomBuilder t m
              , Prerender js t m
              , MonadHold t m
              , MonadFix m
              , PostBuild t m
              , MonadFix m) => m ()
reqListaCont = do
  r <- workflow reqTabela
  el "div" (dynText r)

tabContainer :: DomBuilder t m => Containers -> m ()
tabContainer ct = do
  el "tr" $ do
    el "td" (text $ T.pack $ show $ idContainer ct)
    el "td" (text $ nomeContainer ct)
    el "td" (text $ tipoContainer ct)
    el "td" (text $ statusContainer ct)
    el "td" (text $ categoriaContainer ct)

home :: DomBuilder t m => m ()
home = do
  elAttr "main" ("class" =: "center") $ do
    elAttr "div" ("class" =: "container") $ do
      el "h2" $ text "Tópicos Especiais em Sistemas para Internet III - 6º Ciclo SI"
      el "ul" $ do
        el "li" $ text "Lucas Favoreto Novoa Otero Machado da Costa"
        el "li" $ text "Lucas Santos Santana da Silva"

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
      el "h2" $ text "Destinos"
      elAttr "img" ("src" =: static @"images.jpeg") blank
      el "h3" $ text "Página em Construção"

data Pagina = Pagina1 | Pagina2 | Pagina3 | Pagina4 | Pagina5 | Pagina6 | Pagina7

clickLi :: DomBuilder t m => Pagina -> T.Text -> m (Event t Pagina)
clickLi p t = do
  (ev, _) <- el' "li" (elAttr "a" ("href" =: "#") (text t))
  return ((\_ -> p) <$> domEvent Click ev)

menuLi :: (DomBuilder t m, MonadHold t m) => m (Dynamic t Pagina)
menuLi = do
  evs <- elAttr "ul" ("class" =: "d-flex") $ do
    p1 <- clickLi Pagina1 "Home"
    p2 <- clickLi Pagina2 "Containers"
    p3 <- clickLi Pagina3 "Inserir Container"
    p4 <- clickLi Pagina4 "Movimentações"
    p5 <- clickLi Pagina4 "Inserir Movimentação"
    p6 <- clickLi Pagina6 "Destions"
    p7 <- clickLi Pagina7 "Inserir Destino"
    return (leftmost [p1, p2, p3, p4, p5, p6, p7])
  holdDyn Pagina1 evs

currPag :: (DomBuilder t m, MonadHold t m, PostBuild t m, Prerender js0 t m, MonadFix m) 
        => Pagina -> m ()
currPag p = 
    case p of
          Pagina1 -> home
          Pagina2 -> reqListaCont
          Pagina3 -> reqCont
          Pagina4 -> home
          Pagina5 -> home
          Pagina6 -> home
          Pagina7 -> home

mainPag :: (DomBuilder t m, MonadHold t m, PostBuild t m, Prerender js0 t m, MonadFix m) => m ()
mainPag = do
  pag <- el "header" $ do
    elAttr "div" ("class" =: "container") $ do
      el "h1" $ text "Projeto Haskell"
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