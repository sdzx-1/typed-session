{-# LANGUAGE OverloadedStrings #-}
-- Echo client program
module Main (main) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Book3.Codec (socketAsChannel, myTracer, encodeMsg, decodeMsg)
import qualified Data.IntMap as IntMap
import TypedSession.Core (SingToInt(singToInt))
import qualified TypedSession.Codec as C
import Book3.Type
import Control.Concurrent.Class.MonadSTM
import TypedSession.Driver (driverSimple)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Class.MonadFork
import TypedSession.Driver
import TypedSession.Codec (Decode(..))
import Control.Effect.Labelled (runLabelledLift)
import Control.Carrier.Random.Gen (runRandom)
import System.Random (newStdGen)
import Data.Functor (void)
import Book3.Peer
import Book3.Protocol

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
       buyer2Tvar  <- newTVarIO IntMap.empty
       let buyerChannel = socketAsChannel sock
           sendMap = IntMap.fromList [(singToInt SBuyer, C.send buyerChannel)]
           buyer2Driver = driverSimple (myTracer "buyer2 :") encodeMsg sendMap buyer2Tvar liftIO

       -- fork buyer decode thread, buyer -> buyer2
       thid <- forkIO $ decodeLoop (myTracer "buyer2 :") Nothing (Decode decodeMsg) buyerChannel buyer2Tvar

       g <- newStdGen

       void $ runLabelledLift $ runRandom g $ runPeerWithDriver buyer2Driver buyer2Peer

       killThread thid