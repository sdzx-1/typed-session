{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Type where

import Data.Kind
import Data.SR

data SBool :: Bool -> Type where
  STrue :: SBool True
  SFalse :: SBool False

type instance Sing = SBool
