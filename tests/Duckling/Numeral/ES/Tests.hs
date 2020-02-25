-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


module Duckling.Numeral.ES.Tests
  ( tests ) where

import Prelude
import Data.String
import Test.Tasty

import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Numeral.ES.Corpus
import Duckling.Testing.Asserts
import Duckling.Testing.Types hiding (examples)
import qualified Duckling.Numeral.ES.AR.Corpus as AR
import qualified Duckling.Numeral.ES.CL.Corpus as CL
import qualified Duckling.Numeral.ES.CO.Corpus as CO
import qualified Duckling.Numeral.ES.ES.Corpus as ES
import qualified Duckling.Numeral.ES.MX.Corpus as MX
import qualified Duckling.Numeral.ES.PE.Corpus as PE
import qualified Duckling.Numeral.ES.VE.Corpus as VE
import qualified Duckling.Region as R
  ( Region
      ( AR
      , ES
      )
  )

tests :: TestTree
tests = testGroup "ES Tests"
  [ makeCorpusTest [This Numeral] corpus
  , localeTests
  ]

localeTests :: TestTree
localeTests = testGroup "Locale Tests"
  [ testGroup "ES_AR Tests"
    [ makeCorpusTest [This Numeral] $ withLocale corpus localeAR AR.allExamples
    ]
  , testGroup "ES_CL Tests"
    [ makeCorpusTest [This Numeral] $ withLocale corpus localeCL CL.allExamples
    ]
  , testGroup "ES_CO Tests"
    [ makeCorpusTest [This Numeral] $ withLocale corpus localeCO CO.allExamples
    ]
  , testGroup "ES_ES Tests"
    [ makeCorpusTest [This Numeral] $ withLocale corpus localeES ES.allExamples
    ]
  , testGroup "ES_MX Tests"
    [ makeCorpusTest [This Numeral] $ withLocale corpus localeMX MX.allExamples
    ]
  , testGroup "ES_PE Tests"
    [ makeCorpusTest [This Numeral] $ withLocale corpus localePE PE.allExamples
    ]
  , testGroup "ES_VE Tests"
    [ makeCorpusTest [This Numeral] $ withLocale corpus localeVE VE.allExamples
    ]
  ]
  where
    localeAR = makeLocale ES $ Just R.AR
    localeCL = makeLocale ES $ Just CL
    localeCO = makeLocale ES $ Just CO
    localeES = makeLocale ES $ Just R.ES
    localeMX = makeLocale ES $ Just MX
    localePE = makeLocale ES $ Just PE
    localeVE = makeLocale ES $ Just VE
