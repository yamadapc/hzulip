hzulip
======
[![Build Status](https://travis-ci.org/yamadapc/hzulip.svg?branch=master)](https://travis-ci.org/yamadapc/hzulip)
[![Hackage](https://img.shields.io/hackage/v/hzulip.svg)](https://hackage.haskell.org/package/hzulip)
[![Dependencies Status](http://img.shields.io/hackage-deps/v/hzulip.svg)](http://packdeps.haskellers.com/feed?needle=hzulip)
[![Gitter chat](https://badges.gitter.im/yamadapc/hzulip.png)](https://gitter.im/yamadapc/hzulip)
- - -
A haskell wrapper for the zulip API. This is very much a work in progress.

## Installing
Simply installing through cabal with `cabal install hzulip` should do it.

## Usage
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

    -- Listening for events works with a callback based API. More
    -- complex patterns for concurrent message handling can be created
    -- from it. As long as your zulip user is already subscribed to
    -- streams, this is all you have to do:
    onNewEvent ["message"] $ \msg -> do
        liftIO $ putStrLn "Got a new message!"
        let usr = messageSender msg
            fn = userFullName usr
            e = userEmail usr

       sendPrivateMessage [e] $ "Thanks for the message " ++ fn ++ "!!"
```

## Documentation
The best resource on this is naturally its haddock documentation, available at
[yamadapc.github.io/hzulip](https://yamadapc.github.io/hzulip). You might also
be interested in the [zulip API documentation](https://zulip.com/api/) as well.

There's also an example bot, which does remote code evaluation on the
`bot-example` directory. It's far from perfect/complete, but since I really have
no interest in writting bots, as much as I'd have in simply hooking existing
tools into `zulip`, it's left as is. It shouldn't be too hard to improve on it
and build something that's actually useful.

Now there's also a better implementation of the example evaluation bot at
[zulip-eval-bot](https://github.com/yamadapc/zulip-eval-bot).

They are currently using an older version of the `hzulip` package though.

## License
This code is licensed under the GPLv2 license for Pedro Tacla Yamada. Plese
refer to [LICENSE](/LICENSE) for more information.
