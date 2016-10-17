module Conduit.UDP where
import Data.Void (Void)
import Data.IOData (IOData)
import Data.MonoTraversable (MonoFoldable)
import System.IO (IOMode(ReadMode, WriteMode), Handle)
import Network.Socket (Socket, SockAddr(SockAddrInet), PortNumber, setSocketOption, socketToHandle)
import qualified Network.Socket as Socket
import Conduit (ConduitM, MonadResource, sinkIOHandle, sourceIOHandle)


data SocketType
    = SourceSocket
    | SinkSocket

ioMode :: SocketType -> IOMode
ioMode socketType =
    case socketType of
        SourceSocket -> ReadMode
        SinkSocket -> WriteMode

socketMode :: SocketType -> Socket -> SockAddr -> IO ()
socketMode socketType =
    case socketType of
        SourceSocket -> Socket.bind
        SinkSocket -> Socket.connect


udpSink
    :: (MonadResource m, IOData a)
    => Prelude.String
    -> PortNumber
    -> ConduitM a Void m ()
udpSink host port =
    sinkIOHandle (udpHandle host port SinkSocket)


udpSource
    :: (MonadResource m, IOData a, MonoFoldable a)
    => Prelude.String
    -> PortNumber
    -> ConduitM () a m ()
udpSource host port =
    sourceIOHandle (udpHandle host port SourceSocket)


udpHandle :: Prelude.String -> PortNumber -> SocketType -> IO Handle
udpHandle host port socketType = do
    socket <- Socket.socket Socket.AF_INET Socket.Datagram Socket.defaultProtocol
    setSocketOption socket Socket.ReusePort 1
    address <- Socket.inet_addr host
    socketMode socketType socket (SockAddrInet port address)
    socketToHandle socket (ioMode socketType)
