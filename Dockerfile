FROM haskell:7.8

RUN cabal update

ADD ./hzulip.cabal /opt/hzulip/hzulip.cabal
RUN cd /opt/hzulip && cabal install --only-dependencies -j4

ADD ./ /opt/hzulip
RUN cd /opt/hzulip && cabal install -j4

WORKDIR /opt/bot
