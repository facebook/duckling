
module Duckling.Time.TR.Tests
  ( tests ) where

import Data.String
import Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Asserts
import Duckling.Testing.Types (testContext, testOptions)
import Duckling.Time.TR.Corpus

tests :: TestTree
tests = testGroup "TR Tests"
  [ makeCorpusTest [Seal Time] corpus
  ]