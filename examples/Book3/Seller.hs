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
import Control.Carrier.Lift (runM)

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
    sellerDriver <- driverSimple (myTracer "seller :") encodeMsg (Decode decodeMsg) [(SomeRole SBuyer, socketAsChannel sock)] liftIO
    g <- newStdGen
    void $ runM $ runRandom g $ runState @Int 0 $ runPeerWithDriver sellerDriver sellerPeer
