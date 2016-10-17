module Conduit.UDP
    ( udpSink
    , udpSource
    ) where

import Data.Void (Void)
import Data.IOData (IOData)
import Data.MonoTraversable (MonoFoldable)
import System.IO (IOMode(ReadMode, WriteMode), Handle)
import qualified Network.Socket as Socket
import Network.Socket
    ( Socket
    , SockAddr(SockAddrInet)
    , PortNumber
    , setSocketOption
    , socketToHandle
    )
import Conduit
    ( ConduitM
    , MonadResource
    , sinkIOHandle
    , sourceIOHandle
    )

-- | A type to distinguish when we want to read and write from sockets,
-- simplifying the implementation
data SocketType
    = SourceSocket -- ^ source socket, should 'Network.Socket.bind' and have 'System.IO.ReadMode'
    | SinkSocket -- ^ sink socket, should 'Network.Socket.connect' and have 'System.IO.WriteMode'

-- | Get the correct 'System.IO.IOMode' for handles representing either source
-- sockets or sink sockets
ioMode :: SocketType -> IOMode
ioMode socketType =
    case socketType of
        SourceSocket -> ReadMode
        SinkSocket -> WriteMode

-- | Use either 'Network.Socket.bind' or 'Network.Socket.connect' depending on the 'SocketType'
socketMode :: SocketType -> Socket -> SockAddr -> IO ()
socketMode socketType =
    case socketType of
        SourceSocket -> Socket.bind
        SinkSocket -> Socket.connect


-- | Fire-and-forget style UDP sink. It will attempt to connect to the
-- specified destination, and if it succeeds it will start hurdling data
-- from upstream toward the destination. Must be used with
-- 'Data.Conduit.runConduitRes', since it uses a finite resource (sockets)
udpSink
    :: (MonadResource m, IOData a)
    => String -- ^ address to destination
    -> PortNumber -- ^ port number on destination
    -> ConduitM a Void m () -- ^ Can be viewed as a @'Data.Conduit.Sink' ByteString@
udpSink host port =
    sinkIOHandle (udpHandle host port SinkSocket)


-- | Fire-and-forget style UDP source. It will attempt to listen on the
-- specified interface and port, and if it succeeds it can read contents
-- being sent to that interface and port. Must be used with
-- 'Data.Conduit.runConduitRes', since it uses a finite resource (sockets)
udpSource
    :: (MonadResource m, IOData a, MonoFoldable a)
    => String -- ^ address to bind to
    -> PortNumber -- ^ port number to bind to
    -> ConduitM () a m () -- ^ Can be viewed as a @'MonadIO m => Data.Conduit.Source m ByteString'@
udpSource host port =
    sourceIOHandle (udpHandle host port SourceSocket)


-- | Given a host, port and 'SocketType', create a socket and
-- 'Network.Socket.connect' or 'Network.Socket.bind' to the address pair.
udpHandle
    :: String -- ^ IP address to host
    -> PortNumber
    -> SocketType
    -> IO Handle
udpHandle host port socketType = do
    socket <- Socket.socket Socket.AF_INET Socket.Datagram Socket.defaultProtocol
    setSocketOption socket Socket.ReusePort 1
    address <- Socket.inet_addr host
    socketMode socketType socket (SockAddrInet port address)
    socketToHandle socket (ioMode socketType)
