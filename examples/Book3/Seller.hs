{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- Echo client program
module Main (main) where

import Book3.Codec (decodeMsg, encodeMsg, myTracer, socketAsChannel)
import Book3.Peer
import Book3.Protocol
import Book3.Type
import Control.Carrier.Random.Gen (runRandom)
import Control.Carrier.State.Strict (runState)
import Control.Concurrent.Class.MonadSTM
import Control.Effect.Labelled (runLabelledLift)
import qualified Control.Exception as E
import Control.Monad.Class.MonadFork
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as C
import Data.Functor (void)
import qualified Data.IntMap as IntMap
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Random (newStdGen)
import TypedSession.Codec (Decode (..))
import qualified TypedSession.Codec as C
import TypedSession.Core (SingToInt (singToInt))
import TypedSession.Driver
import TypedSession.Driver (driverSimple)

main :: IO ()
main = runTCPClient "127.0.0.1" "3000"

runTCPClient :: HostName -> ServiceName -> IO ()
runTCPClient host port = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close client
 where
  resolve = do
    let hints = defaultHints{addrSocketType = Stream}
    head <$> getAddrInfo (Just hints) (Just host) (Just port)

  open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
    connect sock $ addrAddress addr
    return sock

  client sock = do
    sellerTvar <- newTVarIO IntMap.empty
    let buyerChannel = socketAsChannel sock
        sendMap = IntMap.fromList [(singToInt SBuyer, C.send buyerChannel)]
        sellerDriver = driverSimple (myTracer "seller :") encodeMsg sendMap sellerTvar liftIO

    -- fork seller decode thread, buyer -> seller
    thid <- forkIO $ decodeLoop (myTracer "seller :") Nothing (Decode decodeMsg) buyerChannel sellerTvar

    g <- newStdGen

    void $ runLabelledLift $ runRandom g $ runState @Int 0 $ runPeerWithDriver sellerDriver sellerPeer

    killThread thid