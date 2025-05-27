FROM debian:bullseye-slim

ENV DEBIAN_FRONTEND=noninteractive

# Install system dependencies: git, curl, cabal, ghc, Python
RUN apt-get update && apt-get install -y \
    locales \
    build-essential \
    git \
    curl \
    libblas-dev \
    liblapack-dev \
    python3 \
    python3-pip \
    ghc \
    cabal-install && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# Clone your repo into the image
RUN git clone https://github.com/qafny/qaskell.git /qaskell

# Copy and install Python dependencies
COPY requirements.txt .
RUN pip3 install --no-cache-dir -r requirements.txt

# Set working directory to the nested Haskell project
WORKDIR /qaskell/qaskell-submission/qaskell

# Build Haskell dependencies and compile the project
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 sh && \
    . /root/.ghcup/env && \
    ghcup install ghc latest && \
    ghcup set ghc latest && \
    cabal update && cabal build

COPY bin .

# Set UTF-8 as default locale
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US:en
ENV LC_ALL=en_US.UTF-8

CMD bash


