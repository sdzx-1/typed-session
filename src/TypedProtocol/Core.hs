{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module TypedProtocol.Core where

import Data.IFunctor
import Data.Kind
import Data.SR

class Protocol role' ps where
  type Done (sr :: role') :: ps -------------------- -- start         send recv
  data Msg role' ps (send :: role') (recv :: role') (from :: ps) (to :: (ps, ps))

data Recv role' ps recv from to where
  Recv
    :: Msg role' ps send recv from '(sps, rps)
    -> Recv role' ps recv from rps

data Agency role' ps r st where
  Agency :: role' -> Sing (r :: role') -> Sing (st :: ps) -> Agency role' ps r (st :: ps)

data SomeMsg role' ps recv from where
  SomeMsg :: Recv role' ps recv from to -> SomeMsg role' ps recv from

data Peer role' ps (r :: role') (m :: Type -> Type) (ia :: ps -> Type) (st :: ps) where
  IReturn :: ia st -> Peer role' ps r m ia st
  LiftM :: m (Peer role' ps r m ia st') -> Peer role' ps r m ia st
  Yield :: (Reify recv, SingI recv, SingI from) => Msg role' ps send recv from '(sps, rps) -> Peer role' ps send m ia sps -> Peer role' ps send m ia from
  Await :: (Reify recv, SingI recv, SingI from) => (Recv role' ps recv from ~> Peer role' ps recv m ia) -> Peer role' ps recv m ia from

instance (Functor m) => IMonadFail (Peer role' ps r m) where
  fail = error

instance (Functor m) => IFunctor (Peer role' ps r m) where
  imap f = \case
    IReturn ia -> IReturn (f ia)
    LiftM f' -> LiftM (fmap (imap f) f')
    Yield ms cont -> Yield ms (imap f cont)
    Await cont -> Await (imap f . cont)

instance (Functor m) => IMonad (Peer role' ps r m) where
  ireturn = IReturn
  ibind f = \case
    IReturn ia -> (f ia)
    LiftM f' -> LiftM (fmap (ibind f) f')
    Yield ms cont -> Yield ms (ibind f cont)
    Await cont -> Await (ibind f . cont)

yield :: (Functor m, Reify recv, SingI recv, SingI from) => Msg role' ps send recv from '(sps, rps) -> Peer role' ps send m (At () sps) from
yield msg = Yield msg (returnAt ())

await :: (Functor m, Reify recv, SingI recv, SingI from) => Peer role' ps recv m (Recv role' ps recv from) from
await = Await ireturn

liftm :: (Functor m) => m a -> Peer role' ps r m (At a ts) ts
liftm m = LiftM (returnAt <$> m)
