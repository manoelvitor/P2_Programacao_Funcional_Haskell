
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}

module Frontend where

import Control.Monad
import qualified Data.Text as T
import Language.Javascript.JSaddle (eval, liftJSM)
import Control.Monad.Fix
import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static
import Data.Map (Map)
import Reflex.Dom.Core
import Text.Read
import Data.Maybe
import Common.Api
import Common.Route
import Data.Aeson

data Pagina = Pagina0 | Pagina1 | Pagina2 | Pagina3 | Pagina4 | Pagina5 | Pagina6

-- para obter o /cliente, /produto, /buscar/27,...
getPath :: R BackendRoute -> T.Text
getPath r = renderBackendRoute checFullREnc r


sendRequest :: ToJSON a => R BackendRoute -> a -> XhrRequest T.Text
sendRequest r dados = postJson (getPath r) dados

reqProd :: ( DomBuilder t m
           , Prerender js t m
           ) => m ()
reqProd = do
  

    el "h1" (text "Cadastro de funcionarios")
    elAttr "p" ("class" =: "title") (text "Nome:") 
    nome <- inputElement def
    elAttr "p" ("class" =: "title") (text "Salario:") 
    salario <- numberInput
    elAttr "p" ("class" =: "title") (text "Cargo:") 
    cargo <- inputElement def
    el "br" (blank)
    el "br" (blank)

    let func = fmap (\((n,c),s) -> Funcionario 0 n s c) (zipDyn (zipDyn (_inputElement_value nome) (_inputElement_value cargo)) salario)
    (submitBtn,_) <- el' "button" (text "Inserir")
    let click = domEvent Click submitBtn
    let funcEvt = tag (current func) click
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_Funcionario :/ ()) <$> funcEvt))
    return () 

req :: ( DomBuilder t m
       , Prerender js t m
       ) => m ()
req = do
    inputEl <- inputElement def
    (submitBtn,_) <- el' "button" (text "Inserir")
    let click = domEvent Click submitBtn
    let nm = tag (current $ _inputElement_value inputEl) click
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_Cliente :/ ()) <$> nm))
    return () 

-----------------------------------------------

getListReq :: XhrRequest ()
getListReq = xhrRequest "GET" (getPath (BackendRoute_Listar :/ ())) def    

data Acao = Perfil Int | Editar Int

tabRegistro :: (PostBuild t m, DomBuilder t m) => Dynamic t Funcionario -> m (Event t Acao)
tabRegistro pr = do 
    el "tr" $ do
        el "td" (dynText $ fmap (T.pack . show . funcionarioId) pr)
        el "td" (dynText $ fmap (T.pack . show . nome) pr)
        el "td" (dynText $ fmap (T.pack . show . salario) pr)
        el "td" (dynText $ fmap (T.pack . show . cargo) pr)  
        evt <- fmap (fmap (const Perfil)) (button "detalhes")
        evt2 <- fmap (fmap (const Editar)) (button "editar")
        
        return (attachPromptlyDynWith (flip ($)) (fmap funcionarioId pr) (leftmost [evt,evt2]))
        
reqTabela :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Workflow t m T.Text
reqTabela = Workflow $ do
    btn <- button "Listar"
    prods :: Dynamic t (Event t (Maybe [Funcionario])) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const getListReq <$> btn))
    evt <- return (fmap (fromMaybe []) $ switchDyn prods)
    dynP <- foldDyn (++) [] evt
    tb <- el "table" $ do
        el "thead" $ do
            el "tr" $ do
                el "th" (text "Id")
                el "th" (text "Nome")
                el "th" (text "Salário")
                el "th" (text "Cargo")
                el "th" (text "Ações")
        
        el "tbody" $ do
             simpleList dynP tabRegistro
    tb' <- return $ switchDyn $ fmap leftmost tb
    return ("Listagem", escolherPag <$> tb')
    where
        escolherPag (Perfil pid) = pagPerfil pid
        escolherPag (Editar pid) = editarPerfil pid

getProdReq :: Int -> XhrRequest ()
getProdReq pid = xhrRequest "GET" (getPath (BackendRoute_Buscar :/ pid)) def        
        
pagPerfil :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
pagPerfil pid = Workflow $ do
    el "br" (blank)     

    btn <- button "mostrar"
    prod :: Dynamic t (Event t (Maybe Funcionario)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const (getProdReq pid) <$> btn))
    mdyn <- holdDyn Nothing (switchDyn prod)
    dynP <- return ((fromMaybe (Funcionario 0 "" 0 "")) <$> mdyn)
    el "div" $ do
        el "div" (dynText $ fmap nome dynP)
        el "div" (dynText $ fmap (T.pack . show . salario) dynP)
        el "div" (dynText $ fmap cargo dynP)
    ret <- button "voltar"
    return ("Perfil: " <> (T.pack $ show pid), reqTabela <$ ret)        

numberInputDyn :: (DomBuilder t m, Num a, Read a, Show a) =>
               Event t a -> m (Dynamic t a)
numberInputDyn p = do
      val <- return (fmap (T.pack . show) p)
      n <- inputElement $ def
        & inputElementConfig_setValue .~ val
      return $ fmap (fromMaybe 0 . readMaybe . T.unpack) 
                 (_inputElement_value n)

editarPerfil :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
editarPerfil pid = Workflow $ do
    btn <- button "mostrar"
    prod :: Dynamic t (Event t (Maybe Funcionario)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync
           (const (getProdReq pid) <$> btn))
    mdyn <- return (switchDyn prod)
    dynE <- return ((fromMaybe (Funcionario 0 "" 0 "")) <$> mdyn)
    elAttr "p" ("class" =: "title") (text "Nome:") 
    nome <- inputElement $ 
         def & inputElementConfig_setValue .~ (fmap nome dynE)
    elAttr "p" ("class" =: "title") (text "Salário:")      
    salario <- numberInput
    elAttr "p" ("class" =: "title") (text "Cargo:")      
    cargo <- inputElement $ 
        def & inputElementConfig_setValue .~ (fmap cargo dynE)
    
    let func = fmap (\((n,v),q) -> Funcionario 0 n v q) (zipDyn (zipDyn (_inputElement_value nome) salario) (_inputElement_value cargo) )
    el "br" (blank)  
    el "br" (blank)     
   
    submitBtn <- button "Editar"

    let funcEvt = tag (current func) submitBtn
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> 
            performRequestAsync (sendRequest (BackendRoute_Editar :/ pid) 
            <$> funcEvt)) 
    return ("Perfil: " <> (T.pack $ show pid), reqTabela <$ submitBtn)  
                 
reqLista :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => m ()
reqLista = do
    r <- workflow reqTabela
    el "div" (dynText r)

---------------------------------------------







---------------------------------------------
            
    
countClick :: DomBuilder t m => m (Event t Int)
countClick = do
    (ev, _) <- el' "button" (text "+")
    return $ ((const 1) <$> (domEvent Click ev))
    
pagClick :: (MonadHold t m, PostBuild t m, DomBuilder t m, MonadFix m) => m ()
pagClick = do
    ev <- countClick
    cter <- accumDyn (+) 0 ev
    el "div" (dynText (fmap (T.pack . show) cter))

clickLi :: (DomBuilder t m, PostBuild t m, MonadHold t m) => Pagina -> T.Text -> m (Event t Pagina)
clickLi p t = do
    (ev, _) <- el' "li" (elAttr "a" ("href" =: "#") (text t))
    return $ (const p) <$> (domEvent Click ev)

menuLi :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m (Dynamic t Pagina)
menuLi = do
    evs <- el "ul" $ do
     
    
        li1 <- clickLi Pagina1 "Cadastrar Funcionário"
        li2 <- clickLi Pagina2 "Listar Funcionário"
        return (leftmost [li1, li2])
    holdDyn Pagina0 evs
    
currPag :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, Prerender js t m) => Pagina -> m ()
currPag p = do
    case p of
         Pagina0 -> blank
         Pagina1 -> reqProd
         Pagina2 -> reqLista

mainPag :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, Prerender js t m) => m ()
mainPag = do 
    pagina <- el "div" menuLi
    dyn_ $ currPag <$> pagina 
         
revText :: T.Text -> T.Text
revText t = T.pack (reverse (T.unpack t))
--





numberInput :: (DomBuilder t m, Num a, Read a) => m (Dynamic t a)
numberInput = do
      n <- inputElement $ def
        & inputElementConfig_initialValue .~ "0"
        & inputElementConfig_elementConfig 
        . elementConfig_initialAttributes .~ ("type" =: "number")
      return $ fmap (fromMaybe 0 . readMaybe . T.unpack) 
                 (_inputElement_value n)



frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      el "h1" $ text "P2 PROGRAMAÇÃO FUNCIONAL -  HASK"
      mainPag
  }
