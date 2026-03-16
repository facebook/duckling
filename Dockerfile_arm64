# ==================================================================================================
# === Build duckling library =======================================================================

FROM docker.io/haskell:8 AS builder

RUN apt-get update -qq \
  && apt-get install -qq -y --fix-missing --no-install-recommends \
     libpcre3 libpcre3-dev build-essential pkg-config wget \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# https://docs.haskellstack.org/en/stable/maintainers/releases/#setting-up-an-arm-vm-for-releases
# Using apt-get install haskell-stack installs version 1.7.3 which is too old
RUN wget https://github.com/commercialhaskell/stack/releases/download/v2.1.3/stack-2.1.3-linux-aarch64.tar.gz
RUN tar -xf stack-*-linux-aarch64.tar.gz && \
    cd stack-*-linux-aarch64/ && \
    chmod +x stack && \
    mv stack /usr/bin/
RUN stack --version

RUN mkdir /log/
WORKDIR /duckling/

ADD . .

ENV LANG=C.UTF-8
RUN stack setup --compiler ghc-8.10.7 --system-ghc

# NOTE:`stack build` will use as many cores as are available to build
# in parallel. However, this can cause OOM issues as the linking step
# in GHC can be expensive. If the build fails, try specifying the
# '-j1' flag to force the build to run sequentially.
RUN stack install -j`nproc` --compiler ghc-8.10.7 --system-ghc --test --no-run-tests

# Run tests. If tests are not built in above step, the main library would be rebuilt here as well.
RUN stack test --compiler ghc-8.10.7 --system-ghc

# ==================================================================================================
# === Copy prebuilt files to new image to greatly reduce the image size ============================

FROM docker.io/debian:buster AS base

ENV LANG C.UTF-8

RUN apt-get update -qq \
  && apt-get install -qq -y --no-install-recommends \
     libpcre3 libgmp10 libnuma1 \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

COPY --from=builder /root/.local/bin/duckling-example-exe /usr/local/bin/

# Additional dependencies are required for the following test
RUN apt-get update -qq \
  && apt-get install -y --no-install-recommends \
     curl \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# Run simple test to check if requests are answered
RUN /bin/bash -c "duckling-example-exe -p 8008 --no-access-log --no-error-log & \
  sleep 5 && \
  response=$(curl -XPOST http://0.0.0.0:8008/parse --data 'locale=en_GB&text=42€') && \
  label='{\"value\":42,\"type\":\"value\",\"unit\":\"EUR\"}' && \
  if [[ '$response' == *'$label'* ]] ; then echo 'ok' ; exit 0 ; else echo 'error' ; exit -1 ; fi;"

# Uninstall curl and it's dependencies again
RUN apt-get remove -y curl && apt-get autoremove -y

# Use port 8008 instead of default port, because port 8000 is already in use by portainer
EXPOSE 8008

CMD ["duckling-example-exe", "-p", "8008", "--no-access-log", "--no-error-log"]
