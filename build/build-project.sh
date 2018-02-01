#!/bin/sh

set -ex

TICKER_VERSION=$(grep '^version:' /project/ticker.cabal | awk '{print $2}')

mkdir -p /build/bin
mkdir -p /build/share/man/man1/
mkdir -p /tmp/fpm

(cd /project \
  && stack setup \
  && stack install --local-bin-path=/build/bin \
  && ronn doc/ticker.1.ronn --roff --pipe | gzip > /build/share/man/man1/ticker.1.gz \
  && cd /project/pkg \
  && fpm \
      -s dir \
      -t deb \
      -n ticker \
      -v ${TICKER_VERSION} \
      /build/bin/ticker=/usr/bin/ \
      /build/share/man/man1/ticker.1.gz=/usr/share/man/man1/ \
 )
