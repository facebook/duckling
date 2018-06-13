module Duckling.Numeral.FI.Tests
  ( tests
  ) where

import Data.String
import Prelude
import Test.Tasty

import Duckling.Dimensions.Types
import Duckling.Numeral.FI.Corpus
import Duckling.Testing.Asserts

tests :: TestTree
tests = testGroup "FI Tests"
  [ makeCorpusTest [This Numeral] corpus
  ]
