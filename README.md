# udp-conduit

Simple fire-and-forget style conduit parts (sources/sinks) for UDP traffic.

Here's an example that sends a message to UDP port 8888 on localhost:

    import Conduit
    import Conduit.UDP
    import Data.Text

    message :: Text
    message = "hello, world!\n"

    main :: IO ()
    main =
        runConduitRes (yield message .| encodeUtf8C .| udpSink "127.0.0.1" 8888)

Here's an example where we continuously receive messages on port 5666 on localhost:

    import Conduit
    import Conduit.UDP

    main :: IO ()
    main =
        runConduitRes (udpSource "127.0.0.1" 5666 .| decodeUtf8C .| stdoutC)

Now, here's an example where we receive messages on port 8001, transform them to
upper-case, and then send them on to port 8002.

    import Conduit
    import Conduit.UDP
    import Data.Char (toUpper)

    main :: IO ()
    main =
        runConduitRes (udpSource "127.0.0.1" 8001 .| omapCE toUpper .| udpSink "127.0.0.1" 8002)

Fun, huh? And simple!
