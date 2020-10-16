# We alias cabal to one which caches build artifacts in a per-checkout directory. This helps reduce rebuilds when switching branches.
CABAL = cabal --builddir=dist/$$(git rev-parse --abbrev-ref HEAD)

package = duckling

# We disable optimizations for local builds, to speed them up
.PHONY: configure
configure:
	$(CABAL) configure --ghc-options="-j -O0" --disable-optimization

.PHONY: build
build: configure
	$(CABAL) build

.PHONY: test
test: configure
	$(CABAL) test

.PHONY: haddock
haddock:
	$(CABAL) haddock

.PHONY: clean
clean:
	$(CABAL) clean

.PHONY: ghci
	$(CABAL) repl

.PHONY: ghcid
ghcid: configure build
	ghcid --command '$(CABAL) repl lib:$(package)' --allow-eval --warnings

.PHONY: sdist
sdist:
	$(CABAL) sdist
