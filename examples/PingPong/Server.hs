module Main where

import qualified Control.Exception as E
import qualified Data.IntMap as IntMap
import Network.Socket
import Type
import qualified TypedProtocol.Codec as C
import TypedProtocol.Core (singToInt)
import Control.Concurrent.Class.MonadSTM
import TypedProtocol.Driver (driverSimple, decodeLoop, runPeerWithDriver)
import Control.Monad.Class.MonadFork
import TypedProtocol.Codec (Decode(..))
import Control.Monad (void)

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
              { addrFlags = [AI_PASSIVE],
                addrSocketType = Stream
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

          sendMap =
            IntMap.fromList
              [ (singToInt SClient, C.send clientChannel)
              ]

      serverTvar <- newTVarIO IntMap.empty

      let serverDriver = driverSimple (myTracer "server: ") encodeMsg sendMap serverTvar id

      thid1 <- forkIO $ decodeLoop (myTracer "server: ") Nothing (Decode decodeMsg) clientChannel serverTvar

      void $ runPeerWithDriver serverDriver serverPeer

      killThread thid1

      close client
