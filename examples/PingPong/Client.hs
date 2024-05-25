{-# LANGUAGE OverloadedStrings #-}

-- Echo client program
module Main (main) where

import qualified Control.Exception as E
import Network.Socket
import Control.Concurrent.Class.MonadSTM
import qualified Data.IntMap as IntMap
import Type (socketAsChannel, SRole (..), myTracer, encodeMsg, decodeMsg, clientPeer)
import TypedProtocol.Core
import qualified TypedProtocol.Codec as C
import TypedProtocol.Driver (driverSimple, decodeLoop, runPeerWithDriver)
import Control.Monad.Class.MonadFork
import TypedProtocol.Codec (Decode(..))
import Control.Monad (void)

main :: IO ()
main = runTCPClient "127.0.0.1" "3000"

runTCPClient :: HostName -> ServiceName -> IO ()
runTCPClient host port = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close client
  where
    resolve = do
      let hints = defaultHints {addrSocketType = Stream}
      head <$> getAddrInfo (Just hints) (Just host) (Just port)

    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
      connect sock $ addrAddress addr
      return sock

    client sock = do
      clientTvar  <- newTVarIO IntMap.empty
      let serverChannel = socketAsChannel sock
          sendMap = IntMap.fromList [(singToInt SServer, C.send serverChannel)]
          clientDriver = driverSimple (myTracer "client: ") encodeMsg sendMap clientTvar id

      thid <- forkIO $ decodeLoop (myTracer "client: ") Nothing (Decode decodeMsg) serverChannel clientTvar

      void $ runPeerWithDriver clientDriver (clientPeer 0)

      killThread thid