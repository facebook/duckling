![Duckling Logo](https://github.com/facebookincubator/duckling/raw/master/logo.png)

# Duckling [![Build Status](https://travis-ci.org/facebookincubator/duckling.svg?branch=master)](https://travis-ci.org/facebookincubator/duckling)
Duckling is a Haskell library that parses text into structured data.

```
"the first Tuesday of October"
=> {"value":"2017-10-03T00:00:00.000-07:00","grain":"day"}
```

## Requirements
A Haskell environment is required. We recommend using
[stack](https://haskell-lang.org/get-started).

On macOS you'll need to install PCRE development headers.
The easiest way to do that is with [Homebrew](https://brew.sh/):
```
brew install pcre
```
If that doesn't help, try running `brew doctor` and fix
the issues it finds.

## Quickstart
To compile and run the binary:
```
$ stack build
$ stack exec duckling-example-exe
```
The first time you run it, it will download all required packages.

This runs a basic HTTP server. Example request:
```
$ curl -XPOST http://0.0.0.0:8000/parse --data 'lang=en&text=tomorrow at eight'
```

See `exe/ExampleMain.hs` for an example on how to integrate Duckling in your
project.

## Supported dimensions
Duckling supports many languages, but most don't support all dimensions yet
(we need your help!).

| Dimension | Example input | Example value output
| --------- | ------------- | --------------------
| `AmountOfMoney` | "42€" | `{"value":42,"type":"value","unit":"EUR"}`
| `Distance` | "6 miles" | `{"value":6,"type":"value","unit":"mile"}`
| `Duration` | "3 mins" | `{"value":3,"minute":3,"unit":"minute","normalized":{"value":180,"unit":"second"}}`
| `Email` | "duckling-team@fb.com" | `{"value":"duckling-team@fb.com"}`
| `Numeral` | "eighty eight" | `{"value":88,"type":"value"}`
| `Ordinal` | "33rd" | `{"value":33,"type":"value"}`
| `PhoneNumber` | "+1 (650) 123-4567" | `{"value":"(+1) 6501234567"}`
| `Quantity` | "3 cups of sugar" | `{"value":3,"type":"value","product":"sugar","unit":"cup"}`
| `Temperature` | "80F" | `{"value":80,"type":"value","unit":"fahrenheit"}`
| `Time` | "today at 9am" | `{"values":[{"value":"2016-12-14T09:00:00.000-08:00","grain":"hour","type":"value"}],"value":"2016-12-14T09:00:00.000-08:00","grain":"hour","type":"value"}`
| `Url` | "https://api.wit.ai/message?q=hi" | `{"value":"https://api.wit.ai/message?q=hi","domain":"api.wit.ai"}`
| `Volume` | "4 gallons" | `{"value":4,"type":"value","unit":"gallon"}`

## Extending Duckling
To regenerate the classifiers and run the test suite:
```
$ stack build :duckling-regen-exe && stack exec duckling-regen-exe && stack test
```

It's important to regenerate the classifiers after updating the code and before
running the test suite.

To extend Duckling's support for a dimension in a given language, typically 2
files need to be updated:
* `Duckling/<dimension>/<language>/Rules.hs`
* `Duckling/<dimension>/<language>/Corpus.hs`

Rules have a name, a pattern and a production.
Patterns are used to perform character-level matching (regexes on input) and
concept-level matching (predicates on tokens).
Productions are arbitrary functions that take a list of tokens and return a new
token.

The corpus (resp. negative corpus) is a list of examples that should (resp.
shouldn't) parse. The reference time for the corpus is Tuesday Feb 12, 2013 at
4:30am.

`Duckling.Debug` provides a few debugging tools:
```
$ stack repl --no-load
> :l Duckling.Debug
> debug EN "in two minutes" [This Time]
in|within|after <duration> (in two minutes)
-- regex (in)
-- <integer> <unit-of-duration> (two minutes)
-- -- integer (0..19) (two)
-- -- -- regex (two)
-- -- minute (grain) (minutes)
-- -- -- regex (minutes)
[Entity {dim = "time", body = "in two minutes", value = "{\"values\":[{\"value\":\"2013-02-12T04:32:00.000-02:00\",\"grain\":\"second\",\"type\":\"value\"}],\"value\":\"2013-02-12T04:32:00.000-02:00\",\"grain\":\"second\",\"type\":\"value\"}", start = 0, end = 14}]
```

## License
Duckling is BSD-licensed. We also provide an additional patent grant.
