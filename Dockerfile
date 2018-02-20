FROM buildpack-deps:trusty

ENV DEBIAN_FRONTEND noninteractive

# Install Rust.
ENV RUST_ARCHIVE=rust-nightly-x86_64-unknown-linux-gnu.tar.gz
ENV RUST_DOWNLOAD_URL=https://static.rust-lang.org/dist/$RUST_ARCHIVE

RUN mkdir -p /rust
WORKDIR /rust

RUN curl -fsOSL $RUST_DOWNLOAD_URL \
    && curl -s $RUST_DOWNLOAD_URL.sha256 | sha256sum -c - \
    && tar -C /rust -xzf $RUST_ARCHIVE --strip-components=1 \
    && rm $RUST_ARCHIVE \
    && ./install.sh

ENV LLVM_VERSION -3.9
# Install Python and LLVM.
RUN \
  apt-get update && \
  apt-get install -y python python-dev python-pip python-virtualenv && \
  apt-get install -y llvm${LLVM_VERSION} llvm${LLVM_VERSION}-dev llvm${LLVM_VERSION}-runtime cmake g++6 && \
  rm -rf /var/lib/apt/lists/*

# Configuration Koak.
VOLUME /koak

WORKDIR /koak

# LLVM
ENV PATH="/usr/lib/llvm-3.9/bin/:${PATH}"



