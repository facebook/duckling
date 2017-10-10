FROM samdoshi/haskell-stack

RUN git clone https://github.com/facebookincubator/duckling.git

RUN mkdir /log

WORKDIR /duckling

RUN stack build

ENTRYPOINT stack exec duckling-example-exe
