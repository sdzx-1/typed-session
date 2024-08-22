{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
-- Echo server program
module Main (main) where

import qualified Control.Exception as E
import Control.Monad (void)
import Network.Socket
import Book3.Codec
import TypedSession.Core
import qualified TypedSession.Codec as C
import Book3.Type
import Control.Concurrent.Class.MonadSTM
import qualified Data.IntMap as IntMap
import TypedSession.Driver
import Control.Monad.Class.MonadFork
import TypedSession.Codec (Decode(..))
import Control.Effect.Labelled (runLabelledLift)
import Control.Carrier.Random.Gen (runRandom)
import Book3.Peer
import System.Random (newStdGen)
import Control.Monad.IO.Class (liftIO)
import Book3.Protocol

main :: IO ()
main = runTCPServer Nothing "3000"

runTCPServer :: Maybe HostName -> ServiceName ->  IO ()
runTCPServer mhost port = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close start
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
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
      
      let sendMap = IntMap.fromList 
                    [ (singToInt SSeller, C.send sellerChannel)
                    , (singToInt SBuyer2, C.send buyer2Channel)
                    ]
      buyerTvar <- newTVarIO IntMap.empty

      let buyerDriver = driverSimple (myTracer "buyer :") encodeMsg sendMap buyerTvar liftIO

      -- fork buyer decode thread, seller -> buyer
      thid1 <- forkIO $ decodeLoop (myTracer "buyer :") Nothing (Decode decodeMsg) sellerChannel buyerTvar
      -- fork buyer decode thread, buyer2 -> buyer
      thid2 <- forkIO $ decodeLoop (myTracer "buyer :") Nothing (Decode decodeMsg) buyer2Channel buyerTvar
      
      g <- newStdGen

      void $ runLabelledLift $ runRandom g $ runPeerWithDriver buyerDriver buyerPeer

      killThread thid1

      killThread thid2

      close buyer2
      close seller
