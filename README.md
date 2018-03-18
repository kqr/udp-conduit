# udp-conduit

Simple fire-and-forget style conduit parts (sources/sinks) for UDP traffic.

Here's an example that sends a message to UDP port 8888 on localhost:

    {-# LANGUAGE OverloadedStrings #-}
    import Conduit
    import Conduit.UDP

    main :: IO ()
    main =
        runConduitRes (yield "hello, world!\n" .| encodeUtf8C .| udpSink "127.0.0.1" 8888)

Here's an example where we continuously receive messages on port 5666 on localhost:

    import Conduit
    import Conduit.UDP

    main :: IO ()
    main =
        runConduitRes (udpSourcePlain "127.0.0.1" 5666 500 .| stdoutC)

Now, here's an example where we receive messages on port 8001, encode them as hex
digits, and then send them on to port 8002.

    import Conduit
    import Conduit.UDP
    import Data.Char (toUpper)

    main :: IO ()
    main =
        runConduitRes (udpSourcePlain "127.0.0.1" 8001 500 .| encodeBase16C  .| udpSink "127.0.0.1" 8002)

Fun, huh? And simple!

You can use `udpSource` to get the ip address of the sender together with the message in a
pair `(ByteString, SockAddr)`
