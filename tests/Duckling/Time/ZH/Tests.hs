-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


module Duckling.Time.ZH.Tests
  ( tests
  ) where

import Data.String
import Prelude
import Test.Tasty

import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Testing.Asserts
import Duckling.Testing.Types
import Duckling.Time.ZH.Corpus
import qualified Duckling.Time.ZH.CN.Corpus as CN
import qualified Duckling.Time.ZH.HK.Corpus as HK
import qualified Duckling.Time.ZH.MO.Corpus as MO
import qualified Duckling.Time.ZH.TW.Corpus as TW

tests :: TestTree
tests = testGroup "ZH Tests"
  [ makeCorpusTest [Seal Time] defaultCorpus
  , localeTests
  ]

localeTests :: TestTree
localeTests = testGroup "Locale Tests"
  [ testGroup "ZH_CN Tests"
    [ makeCorpusTest [Seal Time] $ withLocale corpus localeCN CN.allExamples
    ]
  , testGroup "ZH_HK Tests"
    [ makeCorpusTest [Seal Time] $ withLocale corpus localeHK HK.allExamples
    ]
  , testGroup "ZH_MO Tests"
    [ makeCorpusTest [Seal Time] $ withLocale corpus localeMO MO.allExamples
    ]
  , testGroup "ZH_TW Tests"
    [ makeCorpusTest [Seal Time] $ withLocale corpus localeTW TW.allExamples
    ]
  ]
  where
    localeCN = makeLocale ZH $ Just CN
    localeHK = makeLocale ZH $ Just HK
    localeMO = makeLocale ZH $ Just MO
    localeTW = makeLocale ZH $ Just TW
