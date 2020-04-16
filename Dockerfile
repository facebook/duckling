FROM haskell:8 as builder

COPY . .

RUN mkdir /log

WORKDIR /duckling

RUN apt-get update

RUN apt-get install -qq -y libpcre3 libpcre3-dev build-essential --fix-missing --no-install-recommends

ENV LANG=C.UTF-8

RUN stack setup
# NOTE:`stack build` will use as many cores as are available to build
# in parallel. However, this can cause OOM issues as the linking step
# in GHC can be expensive. If the build fails, try specifying the
# '-j1' flag to force the build to run sequentially.

RUN stack build --copy-bins
ENTRYPOINT stack exec duckling-example-exe

FROM haskell:8

WORKDIR /duckling

# Remove git since the installed version has a security leak and isn't required anyway
RUN mkdir /log && apt purge -y git-man git

COPY --from=builder /root/.local/bin/duckling-example-exe .
ENTRYPOINT [ "./duckling-example-exe" ]