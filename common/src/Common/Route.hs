-- {-# LANGUAGE EmptyCase #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE KindSignatures #-}
-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE TypeFamilies #-}
-- module Common.Route where

-- {- -- You will probably want these imports for composing Encoders.
-- import Prelude hiding (id, (.))
-- import Control.Category
-- -}
-- import Data.Text (Text,unpack)
-- import Data.Functor.Identity
-- import Data.Function
-- import Obelisk.Route
-- import Obelisk.Route.TH

-- data BackendRoute :: * -> * where
--   BackendRoute_Cliente :: BackendRoute ()
--   BackendRoute_Missing :: BackendRoute ()
--   BackendRoute_Funcionario :: BackendRoute () 
--   BackendRoute_Listar :: BackendRoute () 
--   BackendRoute_Buscar :: BackendRoute Int



-- data FrontendRoute :: * -> * where
--   FrontendRoute_Main :: FrontendRoute ()
--   -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

-- checFullREnc
--   :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
-- checFullREnc = checkEncoder fullRouteEncoder & \case
--   Left err -> error $ unpack err
--   Right encoder -> encoder    
  
-- fullRouteEncoder
--   :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
-- fullRouteEncoder = mkFullRouteEncoder
--   (FullRoute_Backend BackendRoute_Missing :/ ())
--   (\case
--       BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
--       BackendRoute_Cliente -> PathSegment "cliente" $ unitEncoder mempty
--       BackendRoute_Funcionario -> PathSegment "funcionario" $ unitEncoder mempty
--       BackendRoute_Listar -> PathSegment "listar" $ unitEncoder mempty
--       BackendRoute_Buscar -> PathSegment "buscar" readShowEncoder)
--   (\case
--       FrontendRoute_Main -> PathEnd $ unitEncoder mempty)

-- concat <$> mapM deriveRouteComponent
--   [ ''BackendRoute
--   , ''FrontendRoute
--   ]



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

import Data.Text (Text,unpack)
import Data.Functor.Identity
import Data.Function
import Obelisk.Route
import Obelisk.Route.TH

checFullREnc :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
checFullREnc = checkEncoder fullRouteEncoder & 
    \case 
          Left err -> error $ unpack err
          Right encoder -> encoder

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Cliente :: BackendRoute ()
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Funcionario:: BackendRoute () 
  BackendRoute_Listar  :: BackendRoute () 
  BackendRoute_Buscar  :: BackendRoute Int
  BackendRoute_Editar :: BackendRoute Int

  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_Cliente -> PathSegment "cliente" $ unitEncoder mempty
      BackendRoute_Funcionario -> PathSegment "funcionario" $ unitEncoder mempty
      BackendRoute_Listar  -> PathSegment "listar" $ unitEncoder mempty
      BackendRoute_Buscar  -> PathSegment "buscar" readShowEncoder
      BackendRoute_Editar -> PathSegment "editar" readShowEncoder
      
      
    )
  (\case
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty)

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
