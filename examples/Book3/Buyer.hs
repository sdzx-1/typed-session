{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- Echo server program
module Main (main) where

import Book3.Codec
import Book3.Peer
import Book3.Protocol
import Control.Carrier.Random.Gen (runRandom)
import Control.Effect.Labelled (runLabelledLift)
import qualified Control.Exception as E
import Control.Monad (void)
import Control.Monad.Class.MonadFork
import Control.Monad.IO.Class (liftIO)
import Network.Socket
import System.Random (newStdGen)
import TypedSession.Codec (Decode (..))
import TypedSession.Driver

main :: IO ()
main = runTCPServer Nothing "3000"

runTCPServer :: Maybe HostName -> ServiceName -> IO ()
runTCPServer mhost port = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close start
 where
  resolve = do
    let hints =
          defaultHints
            { addrFlags = [AI_PASSIVE]
            , addrSocketType = Stream
            }
    head <$> getAddrInfo (Just hints) mhost (Just port)

  open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
    setSocketOption sock ReuseAddr 1
    withFdSocket sock setCloseOnExecIfNeeded
    bind sock $ addrAddress addr
    listen sock 1024
    return sock

  start sock = do
    (buyer2, _peer) <- accept sock
    (seller, _peer) <- accept sock

    let buyer2Channel = socketAsChannel buyer2
        sellerChannel = socketAsChannel seller

    buyerDriver <-
      driverSimple
        (myTracer "buyer :")
        encodeMsg
        (Decode decodeMsg)
        [ (SomeRole SBuyer2, buyer2Channel)
        , (SomeRole SSeller, sellerChannel)
        ]
        liftIO

    g <- newStdGen

    void $ runLabelledLift $ runRandom g $ runPeerWithDriver buyerDriver buyerPeer

    close buyer2
    close seller
