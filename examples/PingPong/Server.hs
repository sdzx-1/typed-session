module Main where

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
import TypedSession.Core (singToInt)
import TypedSession.Driver (SomeRole (SomeRole), decodeLoop, driverSimple, runPeerWithDriver)

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
    (client, _peer) <- accept sock
    let clientChannel = socketAsChannel client
    serverDriver <- driverSimple (myTracer "server: ") encodeMsg (Decode decodeMsg) [(SomeRole SClient, clientChannel)] id
    void $ runPeerWithDriver serverDriver serverPeer
    close client
