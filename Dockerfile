FROM ubuntu:14.04

ENV DEBIAN_FRONTEND noninteractive
ENV LANG C.UTF-8

ENV GHC_VERSION 7.6.3

RUN apt-get update && apt-get install -y \
    curl \
    zlib1g-dev \
    make \
    libpq-dev \
    ghc=${GHC_VERSION}*

WORKDIR /tmp

ENV CABAL_INSTALL_VERSION 1.20.0.3
RUN curl -O http://hackage.haskell.org/package/cabal-install-${CABAL_INSTALL_VERSION}/cabal-install-${CABAL_INSTALL_VERSION}.tar.gz
RUN tar xvfz cabal-install-${CABAL_INSTALL_VERSION}.tar.gz
RUN cd cabal-install-${CABAL_INSTALL_VERSION} && ./bootstrap.sh --global --no-doc
ENV PATH /root/.cabal/bin:$PATH
RUN rm -rf cabal-install-${CABAL_INSTALL_VERSION}*

RUN cabal update && cabal install keter
RUN mkdir -p /opt/keter/bin /opt/keter/incoming /opt/keter/etc
RUN cp /root/.cabal/bin/keter /opt/keter/bin/

ADD . /opt/devel/reviewhub
WORKDIR /opt/devel/reviewhub

RUN cabal update && make clean-rebuild

ENV PATH /opt/devel/reviewhub/.cabal-sandbox/bin:$PATH
RUN yesod keter && mv *.keter /opt/keter/incoming/
RUN cp config/keter-config.yaml /opt/keter/etc/keter-config.yaml

CMD ["/opt/keter/bin/keter", "/opt/keter/etc/keter-config.yaml"]
