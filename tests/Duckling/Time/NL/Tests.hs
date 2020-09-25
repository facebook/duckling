-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


module Duckling.Time.NL.Tests
  ( tests
  ) where

import Data.String
import Prelude
import Test.Tasty

import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Testing.Asserts
import Duckling.Testing.Types
import Duckling.Time.NL.Corpus
import qualified Duckling.Time.NL.BE.Corpus as BE

tests :: TestTree
tests = testGroup "NL Tests"
  [ makeCorpusTest [Seal Time] corpus
  , makeCorpusTest [Seal Time] latentCorpus
  , makeNegativeCorpusTest [Seal Time] negativeCorpus
  , localeTests
  ]

localeTests :: TestTree
localeTests = testGroup "Locale Tests"
  [ testGroup "NL_BE Tests"
    [ makeCorpusTest [Seal Time] $ withLocale corpus localeBE BE.allExamples
    , makeNegativeCorpusTest [Seal Time] $ withLocale negativeCorpus localeBE []
    ]
  ]
  where
    localeBE = makeLocale NL $ Just BE
