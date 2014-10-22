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
main = do
    -- Before doing anything, a `ZulipClient` needs to be created. This will
    -- keep your credentials stored, so you don't have to keep passing them in,
    -- as well as use a single HTTP `Manager`, which should increase the
    -- performance of several subsequent API requests.
    z <- newZulip "bot-email" "api-key"

    -- Sending messages is really straightforward, there are helpers for sending
    -- private and stream messages and a generalized function for constructing
    -- higher-level usage patterns.
    sendPrivateMessage z ["someone-cool"] "Hey, I'm really tired, what's up?"

    -- Listening for events is as easy as registering a callback function. As
    -- long as your bot is subscribed to the events it expects to listen for,
    -- it'll get away with:
    onNewEvent z ["message"] $ \msg -> do
        let usr = messageSender msg
            usrName = userFullName usr
            usrEmail = userEmail usr

        sendPrivateMessage z [usrEmail] $ "Thanks for the message " ++
                                          usrName ++ "!!"

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

## License
This code is licensed under the GPLv2 license for Pedro Tacla Yamada. Plese
refer to [LICENSE](/LICENSE) for more information.
