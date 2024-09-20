{-# LANGUAGE OverloadedStrings #-}

-- Echo client program
module Main (main) where

import Control.Concurrent.Class.MonadSTM
import qualified Control.Exception as E
import Control.Monad (void)
import Control.Monad.Class.MonadFork
import qualified Data.IntMap as IntMap
import Network.Socket
import PingPong.Peer
import PingPong.Protocol
import PingPong.Type
import TypedSession.Codec (Decode (..))
import qualified TypedSession.Codec as C
import TypedSession.Core
import TypedSession.Driver (SomeRole (SomeRole), decodeLoop, driverSimple, runPeerWithDriver)

main :: IO ()
main = runTCPClient

getSocket :: HostName -> ServiceName -> IO Socket
getSocket host port = do
  addr <- resolve
  open addr
 where
  resolve = do
    let hints = defaultHints{addrSocketType = Stream}
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
    let serverChannel = socketAsChannel serverSock
        counterChannel = socketAsChannel countSock
    clientDriver <-
      driverSimple
        (myTracer "client: ")
        encodeMsg
        (Decode decodeMsg)
        [ (SomeRole SServer, serverChannel)
        , (SomeRole SCounter, counterChannel)
        ]
        id

    void $ runPeerWithDriver clientDriver (clientPeer 0)
