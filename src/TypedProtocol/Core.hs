{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module TypedProtocol.Core where

import Data.Dependent.Map (DMap)
import Data.GADT.Compare (GCompare)
import Data.IFunctor
import Data.Kind

class Protocol role' ps where
  type Done (sr :: role') :: ps
  data Msg role' ps (from :: ps) (sendAndSt :: (role', ps)) (recvAndSt :: (role', ps))

data Recv role' ps recv from to where
  Recv
    :: Msg role' ps from '(send, sps) '(recv, rps)
    -> Recv role' ps recv from rps

data SomeMsg role' ps (ts :: ps) where
  SomeMsg
    :: Recv role' ps (recv :: role') (from :: ps) (to :: ps)
    -> SomeMsg role' ps from

type AgencyMsg role' ps = DMap (Sing @ps) (SomeMsg role' ps)

data AnyMsg role' ps where
  AnyMsg
    :: ( SingI recv
       , SingI st
       , GCompare (Sing @role')
       , GCompare (Sing @ps)
       )
    => Msg role' ps st '(send, st') '(recv, st'')
    -> AnyMsg role' ps

msgFromStSing
  :: forall role' ps st send recv st' st''
   . (SingI recv, SingI st, GCompare (Sing @role'))
  => Msg role' ps st '(send, st') '(recv, st'')
  -> Sing st
msgFromStSing _ = sing @st

data Peer role' ps (r :: role') (m :: Type -> Type) (ia :: ps -> Type) (st :: ps) where
  IReturn :: ia st -> Peer role' ps r m ia st
  LiftM :: m (Peer role' ps r m ia st') -> Peer role' ps r m ia st
  Yield
    :: (SingI recv, SingI from, GCompare (Sing @ps))
    => Msg role' ps from '(send, sps) '(recv, rps)
    -> Peer role' ps send m ia sps
    -> Peer role' ps send m ia from
  Await
    :: (SingI recv, SingI from, GCompare (Sing @ps))
    => (Recv role' ps recv from ~> Peer role' ps recv m ia)
    -> Peer role' ps recv m ia from

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

yield
  :: (Functor m, SingI recv, SingI from, GCompare (Sing @ps))
  => Msg role' ps from '(send, sps) '(recv, rps)
  -> Peer role' ps send m (At () sps) from
yield msg = Yield msg (returnAt ())

await
  :: (Functor m, SingI recv, SingI from, GCompare (Sing @ps))
  => Peer role' ps recv m (Recv role' ps recv from) from
await = Await ireturn

liftm :: (Functor m) => m a -> Peer role' ps r m (At a ts) ts
liftm m = LiftM (returnAt <$> m)
