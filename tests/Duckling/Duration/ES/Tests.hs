module Duckling.Duration.ES.Tests
  ( tests
  ) where

import Prelude
import Data.String
import Test.Tasty

import Duckling.Dimensions.Types
import Duckling.Duration.ES.Corpus
import Duckling.Testing.Asserts

tests :: TestTree
tests = testGroup "ES Tests"
  [ makeCorpusTest [This Duration] corpus
  ]
