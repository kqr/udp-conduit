module Conduit.UDP
    ( udpSink
    , udpSource
    , udpSourcePlain
    , sinkSocket
    , sinkIOSocket
    , sourceSocket
    , sourceIOSocket
    ) where

import Data.ByteString (ByteString)
import qualified Network.Socket as Socket
import Network.Socket
    ( Socket
    , SockAddr(SockAddrInet)
    , PortNumber
    , setSocketOption
    )

import Network.Socket.ByteString
   ( recvFrom
    , send
    )
   
import Conduit
    ( ConduitM
    , MonadIO
    , await
    , yield
    , mapC
    , liftIO
    , (.|)
    )

import Control.Monad.Trans.Resource
    ( MonadResource
    , allocate
    )

import Control.Monad (void)

-- | A type to distinguish when we want to read and write from sockets,
-- simplifying the implementation
data SocketType
    = SourceSocket -- ^ source socket, should 'Network.Socket.bind'
    | SinkSocket   -- ^ sink socket, should 'Network.Socket.connect'

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
    :: (MonadResource m)
    => String -- ^ address to destination
    -> PortNumber -- ^ port number on destination
    -> ConduitM ByteString o m ()
udpSink host port =
    sinkIOSocket (udpSocket host port SinkSocket)


-- | Fire-and-forget style UDP source. It will attempt to listen on the
-- specified interface and port, and if it succeeds it can read contents
-- being sent to that interface and port.
-- It streams a pair of (message, source-address) allowing the downstream
-- to know who sent the message. 
-- Must be used with
-- 'Data.Conduit.runConduitRes', since it uses a finite resource (sockets)
udpSource
    :: (MonadResource m)
    => String                                   -- ^ address to bind to
    -> PortNumber                               -- ^ port number to bind to
    -> Int                                      -- ^ the maximum size of a datagram
    -> ConduitM i (ByteString, SockAddr) m ()
udpSource host port sz =
    sourceIOSocket (udpSocket host port SourceSocket) sz


-- | Variant of 'udpSource' streaming only the 'ByteString' messages, without
-- the address of the message source.
udpSourcePlain
    :: (MonadResource m)
    => String                           -- ^ address to bind to
    -> PortNumber                       -- ^ port number to bind to
    -> Int                              -- ^ the maximum size of a datagram
    -> ConduitM i ByteString m ()
udpSourcePlain host port sz =
    udpSource host port sz .| mapC fst


-- | Given a host, port and 'SocketType', create a socket and
-- 'Network.Socket.connect' or 'Network.Socket.bind' to the address pair.
udpSocket
    :: String -- ^ IP address to host
    -> PortNumber
    -> SocketType
    -> IO Socket
udpSocket host port socketType =  do
    socket <- Socket.socket Socket.AF_INET Socket.Datagram Socket.defaultProtocol
    setSocketOption socket Socket.ReusePort 1
    address <- Socket.inet_addr host
    socketMode socketType socket (SockAddrInet port address)
    return socket

-- | Stream all incoming data to the given connected 'Socket'. Note that this function will
-- not automatically close the 'Socket' when processing completes.
sinkSocket :: (MonadIO m) => Socket -> ConduitM ByteString o m ()
sinkSocket s = do
  mmsg <- await
  case mmsg of
    Just msg -> do
      void $ liftIO $ send s msg -- since we send datagrams, can't use sendAll; ignore the value returned by send
      sinkSocket s
    Nothing -> return ()

-- | An alternative to 'sinkSocket'. Instead of taking a pre-opened 'Socket', it
-- takes an action that returns a connected 'Socket' , so that it can open it
-- only when needed and close it as soon as possible.
sinkIOSocket :: MonadResource m => IO Socket -> ConduitM ByteString o m ()
sinkIOSocket connectAction = do
  (_, s) <- allocate connectAction Socket.close         
  sinkSocket s

-- | Stream the contents of a bound 'Socket' as binary data. Note that this function will not automatically
-- close the 'Socket' when processing completes, since it did not acquire the 'Socket' in the first place.
sourceSocket :: MonadIO m
             => Socket                                  -- ^ the socket
             -> Int                                     -- ^ maximum length of datagram to receive
             -> ConduitM i (ByteString, SockAddr) m ()  -- ^ streams downstream the received data and sender address
sourceSocket s sz = do
  msg <- liftIO $ recvFrom s sz
  yield msg
  sourceSocket s sz

-- | An alternative to sourceSocket. Instead of taking a pre-bound 'Socket', it takes an action that opens and
-- binds a Socket, so that it can open it only when needed and close it as soon as possible.
sourceIOSocket :: MonadResource m
               => IO Socket                                     -- ^ the action that creates the socket
               -> Int                                           -- ^ maximum length of datagram to receive
               -> ConduitM i (ByteString, SockAddr) m ()        -- ^ streams downstream the received data and the sender address
sourceIOSocket bindAction sz = do
  (_, s) <- allocate bindAction Socket.close
  sourceSocket s sz

