FROM samdoshi/haskell-stack

RUN git clone https://github.com/facebookincubator/duckling.git

RUN mkdir /log

WORKDIR /duckling

RUN stack build --install-ghc

ENTRYPOINT stack exec duckling-example-exe
