FROM haskell:8

RUN mkdir /log

WORKDIR /duckling

RUN apt-get update

RUN apt-get install -qq -y libpcre3 libpcre3-dev build-essential --fix-missing --no-install-recommends

COPY . .

RUN stack setup

RUN stack build

ENTRYPOINT stack exec duckling-example-exe
