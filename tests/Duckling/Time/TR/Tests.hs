module Duckling.Time.TR.Tests
  ( tests ) where

import Data.String
import Test.Tasty

import Duckling.Dimensions.Types
import Duckling.Testing.Asserts
import Duckling.Time.TR.Corpus

tests :: TestTree
tests = testGroup "TR Tests"
  [ makeCorpusTest [Seal Time] corpus
  ]
