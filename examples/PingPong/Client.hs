{-# LANGUAGE OverloadedStrings #-}

-- Echo client program
module Main (main) where

import Control.Concurrent.Class.MonadSTM
import qualified Control.Exception as E
import Control.Monad (void)
import Control.Monad.Class.MonadFork
import qualified Data.IntMap as IntMap
import Network.Socket
import Type (SRole (..), clientPeer, decodeMsg, encodeMsg, myTracer, socketAsChannel)
import TypedProtocol.Codec (Decode (..))
import qualified TypedProtocol.Codec as C
import TypedProtocol.Core
import TypedProtocol.Driver (decodeLoop, driverSimple, runPeerWithDriver)

main :: IO ()
main = runTCPClient

getSocket :: HostName -> ServiceName -> IO Socket
getSocket host port = do
  addr <- resolve
  open addr
  where
    resolve = do
      let hints = defaultHints {addrSocketType = Stream}
      head <$> getAddrInfo (Just hints) (Just host) (Just port)

    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
      connect sock $ addrAddress addr
      return sock

runTCPClient :: IO ()
runTCPClient = withSocketsDo $ do
  E.bracket
    ( do
        serverSock <- getSocket "127.0.0.1" "3000"
        countSock <- getSocket "127.0.0.1" "3001"
        pure (serverSock, countSock)
    )
    (\(a, b) -> close a >> close b)
    (client)
  where
    client (serverSock, countSock) = do
      clientTvar <- newTVarIO IntMap.empty
      let serverChannel = socketAsChannel serverSock
          counterChannel = socketAsChannel countSock
          sendMap =
            IntMap.fromList
              [ (singToInt SServer, C.send serverChannel),
                (singToInt SCounter, C.send counterChannel)
              ]
          clientDriver = driverSimple (myTracer "client: ") encodeMsg sendMap clientTvar id

      thid1 <- forkIO $ decodeLoop (myTracer "client: ") Nothing (Decode decodeMsg) serverChannel clientTvar
      thid2 <- forkIO $ decodeLoop (myTracer "client: ") Nothing (Decode decodeMsg) counterChannel clientTvar

      void $ runPeerWithDriver clientDriver (clientPeer 0)

      killThread thid1
      killThread thid2