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

module TypedSession.Core where

import Data.IFunctor
import Data.IntMap (IntMap)
import Data.Kind

{- |

typed-session is a communication framework.
The messages received from the outside will be placed in MsgCache.
When interpreting the Peer, (Sing (r :: s)) will be generated according to the Peer's status.
SingToInt can convert Sing (r :: s) to Int.
Depending on this Int value, the required Msg is finally found from MsgCache.

In the process of multi-role communication, a message cache structure like MsgCache is needed.

Consider the following scenario

@
s1   s1   s2       Initial state
------------
a -> b             a sends message MsgA to b
------------
s3   s2   s2       State after sending
------------
     b <- c        c sends message MsgC to b
------------
s3   s4   s5       State after sending
@

For b, due to the influence of network transmission, it cannot guarantee that it can receive MsgA first and then MsgC.
If it receives MsgC first, it will directly put it in MsgCache and continue to wait until
MsgA arrives, then it will start to process MsgA first and then MsgC.

In general, dataToTag# is used directly here.

Example:

@
instance SingToInt Role where
  singToInt x = I# (dataToTag# x)

instance SingToInt PingPong where
  singToInt x = I# (dataToTag# x)
@
-}
class SingToInt s where
  singToInt :: Sing (r :: s) -> Int

{- |

Describes the type class of Msg. The core of typed-session.

@
type Done (sr :: role') :: ps
@

Describe the state of each role when it terminates.

@
data Msg role' ps (from :: ps) (sendAndSt :: (role', ps)) (recvAndSt :: (role', ps))
@
* role': the type of the role.
* ps: the type of the state machine.
* ​​from: when sending a message, the sender is in this state,
where the receiver may be in this state, or a more generalized state related to this state.
For example, the sender is in state (S1 [True]), and the receiver is in state (S1 s).
* sendAndSt: the role that sends the message and the state of the role after sending the message.
* recvAndSt: the role that receives the message and the state of the role after receiving the message.

There are two principles to follow when designing the state of Msg:

1. When sending a message, the sender and receiver must be in the same state. Here the receiver may be in a more generalized state related to the state.
For example, the sender is in state (S1 [True]), and the receiver is in state (S1 s).

2. The same state can only be used for the same pair of receiver and sender.

For example, in the following example, state s1 is used for both (a -> b) and (b -> c), which is wrong.

@
s1   s1   s1
a -> b
s2   s1   s1
     b -> c
s2   s4   s5
@
-}
class (SingToInt role', SingToInt ps) => Protocol role' ps where
  type Done (sr :: role') :: ps
  data Msg role' ps (from :: ps) (sendAndSt :: (role', ps)) (recvAndSt :: (role', ps))

{- |
Package Msg and extract the required type.

@
Msg  role' ps from '(send, sps) '(recv,     rps)
Recv role' ps                     recv from rps
@
-}
data Recv role' ps recv from to where
  Recv
    :: Msg role' ps from '(send, sps) '(recv, rps)
    -> Recv role' ps recv from rps

{- |
Messages received from the outside are placed in MsgCache. When interpreting
Peer will use the Msg in MsgCache.
-}
type MsgCache role' ps = IntMap (AnyMsg role' ps)

{- |
Packaging of Msg, shielding part of the type information, mainly used for serialization.
-}
data AnyMsg role' ps where
  AnyMsg
    :: ( SingI recv
       , SingI st
       , SingToInt role'
       , SingToInt ps
       )
    => Msg role' ps st '(send, st') '(recv, st'')
    -> AnyMsg role' ps

msgFromStSing
  :: forall role' ps st send recv st' st''
   . (SingI recv, SingI st)
  => Msg role' ps st '(send, st') '(recv, st'')
  -> Sing st
msgFromStSing _ = sing @st

{- |
Core Ast, all we do is build this Ast and then interpret it.

@
IReturn :: ia st -> Peer role' ps r m ia st
@
IReturn indicates the termination of the continuation.

@
LiftM :: m (Peer role' ps r m ia st') -> Peer role' ps r m ia st
@

Liftm can transform state st to any state st'.
It looks a bit strange, as if it is a constructor that is not constrained by the Msg type.
Be careful when using it, it is a type breakpoint.
But some state transition functions need it, which can make the code more flexible.
Be very careful when using it!

@
Yield
  :: ( SingI recv
     , SingI from
     , SingToInt ps
     )
  => Msg role' ps from '(send, sps) '(recv, rps)
  -> Peer role' ps send m ia sps
  -> Peer role' ps send m ia from
@
Yield represents sending a message. Note that the Peer status changes from `from` to `sps`.

@
Await
  :: ( SingI recv
     , SingI from
     , SingToInt ps
     )
  => (Recv role' ps recv from ~> Peer role' ps recv m ia)
  -> Peer role' ps recv m ia from
@

Await represents receiving messages.
Different messages will lead to different states.
The state is passed to the next behavior through (~>).
-}
data Peer role' ps (r :: role') (m :: Type -> Type) (ia :: ps -> Type) (st :: ps) where
  IReturn :: ia st -> Peer role' ps r m ia st
  LiftM :: m (Peer role' ps r m ia st') -> Peer role' ps r m ia st
  Yield
    :: ( SingI recv
       , SingI from
       , SingToInt ps
       )
    => Msg role' ps from '(send, sps) '(recv, rps)
    -> Peer role' ps send m ia sps
    -> Peer role' ps send m ia from
  Await
    :: ( SingI recv
       , SingI from
       , SingToInt ps
       )
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

{- |
Send a message, the Peer status changes from `from` to `sps`.
-}
yield
  :: ( Functor m
     , SingI recv
     , SingI from
     , SingToInt ps
     )
  => Msg role' ps from '(send, sps) '(recv, rps)
  -> Peer role' ps send m (At () sps) from
yield msg = Yield msg (returnAt ())

{- |
Receiving Messages.
-}
await
  :: ( Functor m
     , SingI recv
     , SingI from
     , SingToInt ps
     )
  => Peer role' ps recv m (Recv role' ps recv from) from
await = Await ireturn

{- |
Lift any m to Peer role' ps r m, which is an application of LiftM.
Note that the state of `ts` has not changed.
-}
liftm :: (Functor m) => m a -> Peer role' ps r m (At a ts) ts
liftm m = LiftM (returnAt <$> m)

liftConstruct :: (Applicative m) => ia st' -> Peer role' k r m ia st
liftConstruct a = LiftM $ pure $ ireturn a