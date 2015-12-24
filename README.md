hzulip
======
[![Build Status](https://travis-ci.org/yamadapc/hzulip.svg?branch=master)](https://travis-ci.org/yamadapc/hzulip)
[![Hackage](https://img.shields.io/hackage/v/hzulip.svg)](https://hackage.haskell.org/package/hzulip)
[![Dependencies Status](http://img.shields.io/hackage-deps/v/hzulip.svg)](http://packdeps.haskellers.com/feed?needle=hzulip)
[![Gitter chat](https://badges.gitter.im/yamadapc/hzulip.png)](https://gitter.im/yamadapc/hzulip)
- - -
A haskell wrapper for the zulip API.

## Installing
Simply installing through cabal with `cabal install hzulip` should do it.

## Usage
### Getting started
```haskell
import Web.HZulip

main :: IO ()
main = withZulipCreds "zulip-api-user" "zulip-api-key" $ do
    -- Since we are inside the ZulipM ReaderT monad transformer, we
    -- don't need to pass options around. The above function already
    -- created an HTTP manager, for connection pooling and wrapped the
    -- default configuration options with the Monad:
    print =<< getSubscriptions
    -- >> ["haskell"]

    -- Sending messages is as easy as:
    void $ sendStreamMessage "haskell"              -- message stream
                             "hzulip"               -- message topic
                             "Message from Haskell" -- message content

    -- Before receiving messages, our client needs to be subscribed to streams
    addAllSubscriptions

    -- Listening for events works with a callback based API:
    onNewMessage $ \msg -> do
        lift $ putStrLn "Got a new message!"
        let usr = messageSender msg
            fn = userFullName usr
            e = userEmail usr

       sendPrivateMessage [e] $ "Thanks for the message " ++ fn ++ "!!"
```

## Documentation
The best resource on this is naturally its haddock documentation, available at
[yamadapc.github.io/hzulip](https://yamadapc.github.io/hzulip). You might also
be interested in the [zulip API documentation](https://zulip.com/api/) as well.

## Examples
There are a couple of example bots and applications on the
[`examples`](https://github.com/yamadapc/hzulip/tree/master/examples) directory. If you're getting started with Haskell, I'd
suggest looking at the [`ZulipLogger`](https://github.com/yamadapc/hzulip/blob/master/examples/src/ZulipLogger.hs) and
[`ZulipEchoBot`](https://github.com/yamadapc/hzulip/blob/master/examples/src/ZulipEchoBot.hs) which show of basic API usage
with minimal noise from anything else.

## A higher-level API
A [`Conduit`](https://www.fpcomplete.com/user/snoyberg/library-documentation/conduit-overview)
API is provided as well and an "echo" example is available for it in the
examples directory; [here](https://github.com/yamadapc/hzulip/blob/master/examples/src/ZulipConduitBot.hs). You can compare the evented implementation and see which you
like best. Right now using streams to handle output is a bit unhandy, but it
provides a nice composable high-level interface for the events.

- - -
Other than the echo and logger examples; there's also a pretty logger at [`ZulipCli`](https://github.com/yamadapc/hzulip/blob/master/examples/src/ZulipCli.hs).

## A remote evaluation bot
Though slightly outdated, there's a remote evaluation Zulip bot using this
library at [zulip-eval-bot](https://github.com/yamadapc/zulip-eval-bot), which
might be worth taking a look at as well.

## License
This code is licensed under the GPLv2 license for Pedro Tacla Yamada. Plese
refer to [LICENSE](/LICENSE) for more information.
