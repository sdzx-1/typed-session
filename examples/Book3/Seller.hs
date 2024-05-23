
{-# LANGUAGE OverloadedStrings #-}
-- Echo client program
module Main (main) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Codec (socketAsChannel, myTracer, encodeMsg, decodeMsg)
import qualified Data.IntMap as IntMap
import TypedProtocol.Core (SingToInt(singToInt))
import qualified TypedProtocol.Codec as C
import Type
import Control.Concurrent.Class.MonadSTM
import TypedProtocol.Driver (driverSimple)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Class.MonadFork
import TypedProtocol.Driver
import TypedProtocol.Codec (Decode(..))
import Control.Effect.Labelled (runLabelledLift)
import Control.Carrier.Random.Gen (runRandom)
import System.Random (newStdGen)
import Data.Functor (void)
import Peer

main :: IO ()
main = runTCPClient "127.0.0.1" "3000" 

runTCPClient :: HostName -> ServiceName ->  IO ()
runTCPClient host port = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close client
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)

    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return sock

    client sock = do 
       sellerTvar  <- newTVarIO IntMap.empty
       let buyerChannel = socketAsChannel sock
           sendMap = IntMap.fromList [(singToInt SBuyer, C.send buyerChannel)]
           sellerDriver = driverSimple (myTracer "seller :") encodeMsg sendMap sellerTvar liftIO

       -- fork seller decode thread, buyer -> seller
       thid <- forkIO $ decodeLoop (myTracer "seller :") Nothing (Decode decodeMsg) buyerChannel sellerTvar

       g <- newStdGen

       void $ runLabelledLift $ runRandom g $ runPeerWithDriver sellerDriver sellerPeer

       killThread thid