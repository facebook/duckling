{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.FI.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale FI Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "0"
             , "nolla"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "yksi"
             ]
  , examples (NumeralValue 10)
             [ "10"
             , "kymmenen"
             ]
  , examples (NumeralValue 13)
             [ "13"
             , "kolmetoista"
             ]
  , examples (NumeralValue 20)
             [ "20"
             , "kaksikymmentä"
             ]
  , examples (NumeralValue 24)
             [ "24"
             , "kaksikymmentäneljä"
             ]
  , examples (NumeralValue 35)
             [ "35"
             , "kolmekymmentäviisi"
             ]
  , examples (NumeralValue 42)
             [ "42"
             , "neljäkymmentäkaksi"
             ]
  , examples (NumeralValue 99)
             [ "99"
             , "yhdeksänkymmentäyhdeksän"
             ]
  ]
