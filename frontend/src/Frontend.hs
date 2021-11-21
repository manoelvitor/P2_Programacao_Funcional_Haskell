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

-------------- EXEMPLO BACKEND -----------------------

getPath :: T.Text
getPath = renderBackendRoute checFullREnc $ BackendRoute_Cliente :/ ()

nomeRequest :: T.Text -> XhrRequest T.Text
nomeRequest s = postJson getPath (Cliente s)

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
    st <- pagReq 
    tx <- holdDyn "" st
    el "div" (dynText tx)
    el "h3" (text "Insira um nome para cadastrar")

    
------------------------------------------------------

data Pagina = Pagina1 | Pagina2 | Pagina3 | Pagina4

numberInput :: DomBuilder t m => m (Dynamic t Double)
numberInput = do
      n <- inputElement $ def
        & inputElementConfig_initialValue .~ "0"
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
      return $ fmap (fromMaybe 0 . readMaybe . T.unpack) $ _inputElement_value n

caixaSoma :: (DomBuilder t m, PostBuild t m) => m ()
caixaSoma = do
    n1 <- numberInput -- m (Dynamic t Double)
    text " "
    n2 <- numberInput -- m (Dynamic t Double)
    dynText (fmap (T.pack . show) (zipDynWith (+) n1 n2))
   
caixas :: (DomBuilder t m, PostBuild t m) => m ()
caixas = do
    t1 <- inputElement def -- m (Dynamic Text)
    t2 <- inputElement def -- m (Dynamic Text)
    text " "
    dynText (zipDynWith (<>) (_inputElement_value t1) (_inputElement_value t2))
    
{-*
    <div>
        <ul>
            <li> Item 1 </li>
            <li class="class1"> Item 2 </li>
            <li> Item 3 </li>
            <li> Item 4 </li>
        </ul>
    </div>
*-}
lista :: DomBuilder t m => m ()
lista = do
    el "div" $ do
        el "ul" $ do
            el "li" (text "Item 1")
            elAttr "li" ("class" =: "class1") (text "Item 2")
            el "li" (text "Item 3")
            el "li" (text "Item 4")

revText :: T.Text -> T.Text
revText t = T.pack (reverse (T.unpack t))
   
buttonClick :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m (Event t T.Text)
buttonClick = do
    t <- inputElement def
    (e,_) <- el' "button" (text "OK")
    return $ attachPromptlyDynWith const 
                                   (fmap revText (_inputElement_value t)) 
                                   (domEvent Click e)            

home :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m ()
home = do
          el "h1" (text "Seja bem vindo ao nosso trabalho")

    

sumButton :: (DomBuilder t m, PostBuild t m, MonadHold t m) 
          => m (Event t Double)
sumButton = do
    n1 <- numberInput
    text " "
    n2 <- numberInput
    text " "
    (e,_) <- el' "button" (text "OK")
    let dynDouble = zipDynWith (+) n1  n2
    return $ attachPromptlyDynWith const    
                                   dynDouble 
                                   (domEvent Click e)

contato :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m ()
contato = do
         el "div" (text "manoelvitor4g2@gmail.com")

    
clickLi :: DomBuilder t m => Pagina -> T.Text -> m (Event t Pagina)
clickLi p t = do
    (ev, _) <- el' "li" (elAttr "a" ("href" =: "#") (text t))
    return ((\_ -> p) <$> domEvent Click ev)
    
menuLi :: (DomBuilder t m, MonadHold t m) => m (Dynamic t Pagina)
menuLi = do
    evs <- el "ul" $ do
        p1 <- clickLi Pagina1 "Pagina Inicial"
        p2 <- clickLi Pagina2 "Contato"
        p3 <- clickLi Pagina3 "Sobre"
        p4 <- clickLi Pagina4 "Cadastro"
        return (leftmost [p1,p2,p3,p4])
    holdDyn Pagina1 evs    
    
currPag :: (DomBuilder t m, MonadHold t m, PostBuild t m, Prerender js0 t m) => Pagina -> m ()
currPag p = 
    case p of
         Pagina1 -> home
         Pagina2 -> contato
         Pagina3 -> el "div" (text "Trabalho para avaliação p2 de programação funcional")
         Pagina4 -> paginaInsere
         
mainPag :: (DomBuilder t m, MonadHold t m, PostBuild t m, Prerender js0 t m) => m ()
mainPag = do
    pag <- el "div" menuLi
    dyn_ $ currPag <$> pag
-- bootstrap.min.css
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "P2-PROGRAMAÇÃO FUNCIONAL - HASKELL"
     
  , _frontend_body = do
      el "h1" (text "Trabalho em desenvolvimento")
      elAttr "link" ("href" =: static @"main.css"  <>  "type" =: "text/css" <> "rel" =: "stylesheet") blank

      mainPag
  }
