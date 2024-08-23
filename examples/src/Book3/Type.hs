{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Book3.Type where

import Data.IFunctor (Sing, SingI)
import qualified Data.IFunctor as I
import Data.Kind
import GHC.Exts (dataToTag#)
import GHC.Int (Int (I#))
import Language.Haskell.TH.Quote (QuasiQuoter)
import TypedSession.Core
import TypedSession.TH (protocol)

data BookRole = Buyer | Seller | Buyer2
  deriving (Show, Eq, Ord, Enum, Bounded)

data BookBranchSt
  = Finish
  | NotFound
  | Found
  | One
  | Two
  | Support
  | NotSupport
  | Enough
  | NotEnough
  deriving (Show, Eq, Ord, Enum, Bounded)

bookProtocol :: QuasiQuoter
bookProtocol =
  protocol
    @BookRole
    @BookBranchSt
    "Book"
    ''BookRole
    ''BookBranchSt
