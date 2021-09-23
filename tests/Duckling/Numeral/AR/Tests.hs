-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


module Duckling.Numeral.AR.Tests
  ( tests ) where

import Prelude
import Data.String
import Test.Tasty

import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Numeral.AR.Corpus
import Duckling.Testing.Asserts
import Duckling.Testing.Types hiding (examples)
import qualified Duckling.Numeral.AR.EG.Corpus as EG

tests :: TestTree
tests = testGroup "AR Tests"
  [ makeCorpusTest [Seal Numeral] corpus
  , localeTests
  ]

localeTests :: TestTree
localeTests = testGroup "Locale Tests"
  [ testGroup "AR_EG Tests"
    [ makeCorpusTest [Seal Numeral] $ withLocale corpus localeEG EG.allExamples
    ]
  ]
  where
    localeEG = makeLocale AR $ Just EG
