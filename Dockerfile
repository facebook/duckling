FROM samdoshi/haskell-stack

RUN git clone https://github.com/facebookincubator/duckling.git

RUN mkdir /log

WORKDIR /duckling

RUN apt-get update

RUN apt-get install -qq -y libpcre3 libpcre3-dev build-essential --fix-missing --no-install-recommends

RUN stack setup --install-ghc
# NOTE:`stack build` will use as many cores as are available to build
# in parallel. However, this can cause OOM issues as the linking step
# in GHC can be expensive. If the build fails, try specifying the
# '-j1' flag to force the build to run sequentially.
RUN stack build

ENTRYPOINT stack exec duckling-example-exe
