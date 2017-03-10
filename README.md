# Duckling
Duckling is a Haskell library that parses text into structured data.

## Requirements
A Haskell environment is required. We recommend using
[stack](https://haskell-lang.org/get-started).

## Quickstart
To compile and run the binary:
```
$ stack build
$ stack exec duckling-example-exec
```
The first time you run it, it will download all required packages.

To run a source file directly (after compiling once):
```
$ stack ExampleMain.hs
```

See `ExampleMain.hs` for an example on how to integrate Duckling in your
project.

To regenerate the classifiers and run the tests:
```
$ stack RegenMain.hs && stack TestMain.hs
```

## License
Duckling is BSD-licensed. We also provide an additional patent grant.
