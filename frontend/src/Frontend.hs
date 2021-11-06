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

menu :: DomBuilder t m => m ()
menu = do
    el "menu" $ do
        el "ul" $ do
            el "li" $ do
                el "a" (text "Home")
            el "li" $ do
                el "a" (text "Sobre")
            el "li" $ do
                el "a" (text "Contato")
            el "li" $ do
                el "a" (text "Links")
   
   
        





frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Página Inicial"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      menu
      elAttr "div" ("class" =: "container") $ do
      el "h1" (text "Hello World!")
      elAttr "img" ("src" =: static @"world.png") blank

 

      
      return ()
  }