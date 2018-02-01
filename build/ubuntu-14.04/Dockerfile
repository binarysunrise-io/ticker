FROM ubuntu:14.04

ENV DEBIAN_FRONTEND noninteractive
ENV HASKELL_PLATFORM_VERSION=8.0.2

# Base userland dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    libgmp-dev \
    wget \
    git

# Haskell platform dependencies
RUN apt-get install -y libz-dev

WORKDIR /tmp/haskell-platform
RUN wget --progress=bar:force https://haskell.org/platform/download/${HASKELL_PLATFORM_VERSION}/haskell-platform-${HASKELL_PLATFORM_VERSION}-unknown-posix--full-x86_64.tar.gz
RUN tar -xzf haskell-platform-${HASKELL_PLATFORM_VERSION}-unknown-posix--full-x86_64.tar.gz
RUN ./install-haskell-platform.sh
RUN rm -rf /tmp/haskell-platform

WORKDIR /tmp

RUN apt-get install -y ruby-dev
RUN gem install --no-ri --no-rdoc fpm  -v '~> 1.8.1'
RUN gem install --no-ri --no-rdoc ronn -v '~> 0.7.3'