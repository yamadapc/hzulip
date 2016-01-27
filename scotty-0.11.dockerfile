FROM haskell:7.8

RUN cabal update

ADD ./hzulip.cabal /opt/hzulip/hzulip.cabal
RUN cd /opt/hzulip && cabal install --enable-test --only-dependencies -j4 --constraint=scotty==0.11.0

ADD ./ /opt/hzulip
RUN cd /opt/hzulip && cabal install -j4

WORKDIR /opt/bot
