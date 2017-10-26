FROM haskell:8

RUN git clone https://github.com/facebookincubator/duckling.git

RUN mkdir /log

WORKDIR /duckling

RUN apt-get update

RUN apt-get install -qq -y libpcre3 libpcre3-dev build-essential --fix-missing --no-install-recommends

RUN stack setup

RUN stack build

ENTRYPOINT stack exec duckling-example-exe
