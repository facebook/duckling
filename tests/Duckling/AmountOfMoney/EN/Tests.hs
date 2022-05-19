-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


module Duckling.AmountOfMoney.EN.Tests
  ( tests
  ) where

import Data.String
import Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Duckling.AmountOfMoney.EN.Corpus
import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Testing.Asserts
import Duckling.Testing.Types (testContext, testOptions, withLocale)
import Duckling.Types (Range(..))
import qualified Duckling.AmountOfMoney.EN.AU.Corpus as AU
import qualified Duckling.AmountOfMoney.EN.BZ.Corpus as BZ
import qualified Duckling.AmountOfMoney.EN.CA.Corpus as CA
import qualified Duckling.AmountOfMoney.EN.GB.Corpus as GB
import qualified Duckling.AmountOfMoney.EN.IN.Corpus as IN
import qualified Duckling.AmountOfMoney.EN.IE.Corpus as IE
import qualified Duckling.AmountOfMoney.EN.JM.Corpus as JM
import qualified Duckling.AmountOfMoney.EN.NZ.Corpus as NZ
import qualified Duckling.AmountOfMoney.EN.PH.Corpus as PH
import qualified Duckling.AmountOfMoney.EN.TT.Corpus as TT
import qualified Duckling.AmountOfMoney.EN.US.Corpus as US
import qualified Duckling.AmountOfMoney.EN.ZA.Corpus as ZA
import qualified Duckling.Region as R

tests :: TestTree
tests = testGroup "EN Tests"
  [ makeCorpusTest [Seal AmountOfMoney] corpus
  , makeNegativeCorpusTest [Seal AmountOfMoney] negativeCorpus
  , localeTests
  , intersectTests
  , rangeTests
  , latentTests
  ]

localeTests :: TestTree
localeTests = testGroup "Locale Tests"
  [ testGroup "EN_AU Tests"
    [ makeCorpusTest [Seal AmountOfMoney]
      $ withLocale corpus localeAU AU.allExamples
    , makeNegativeCorpusTest [Seal AmountOfMoney]
      $ withLocale negativeCorpus localeAU AU.negativeExamples
    ]
   , testGroup "EN_BZ Tests"
    [ makeCorpusTest [Seal AmountOfMoney]
      $ withLocale corpus localeBZ BZ.allExamples
    , makeNegativeCorpusTest [Seal AmountOfMoney]
      $ withLocale negativeCorpus localeBZ BZ.negativeExamples
    ]
   , testGroup "EN_CA Tests"
    [ makeCorpusTest [Seal AmountOfMoney]
      $ withLocale corpus localeCA CA.allExamples
    , makeNegativeCorpusTest [Seal AmountOfMoney]
      $ withLocale negativeCorpus localeCA CA.negativeExamples
    ]
   , testGroup "EN_GB Tests"
    [ makeCorpusTest [Seal AmountOfMoney]
      $ withLocale corpus localeGB GB.allExamples
    , makeNegativeCorpusTest [Seal AmountOfMoney]
      $ withLocale negativeCorpus localeGB GB.negativeExamples
    ]
   , testGroup "EN_IE Tests"
    [ makeCorpusTest [Seal AmountOfMoney]
      $ withLocale corpus localeIE IE.allExamples
    , makeNegativeCorpusTest [Seal AmountOfMoney]
      $ withLocale negativeCorpus localeIE IE.negativeExamples
    ]
   , testGroup "EN_IN Tests"
    [ makeCorpusTest [Seal AmountOfMoney]
      $ withLocale corpus localeIN IN.allExamples
    , makeNegativeCorpusTest [Seal AmountOfMoney]
      $ withLocale negativeCorpus localeIN IN.negativeExamples
    ]
   , testGroup "EN_JM Tests"
    [ makeCorpusTest [Seal AmountOfMoney]
      $ withLocale corpus localeJM JM.allExamples
    , makeNegativeCorpusTest [Seal AmountOfMoney]
      $ withLocale negativeCorpus localeJM JM.negativeExamples
    ]
   , testGroup "EN_NZ Tests"
    [ makeCorpusTest [Seal AmountOfMoney]
      $ withLocale corpus localeNZ NZ.allExamples
    , makeNegativeCorpusTest [Seal AmountOfMoney]
      $ withLocale negativeCorpus localeNZ NZ.negativeExamples
    ]
   , testGroup "EN_PH Tests"
    [ makeCorpusTest [Seal AmountOfMoney]
      $ withLocale corpus localePH PH.allExamples
    , makeNegativeCorpusTest [Seal AmountOfMoney]
      $ withLocale negativeCorpus localePH PH.negativeExamples
    ]
    , testGroup "EN_TT Tests"
    [ makeCorpusTest [Seal AmountOfMoney]
      $ withLocale corpus localeTT TT.allExamples
    , makeNegativeCorpusTest [Seal AmountOfMoney]
      $ withLocale negativeCorpus localeTT TT.negativeExamples
    ]
    , testGroup "EN_US Tests"
    [ makeCorpusTest [Seal AmountOfMoney]
      $ withLocale corpus localeUS US.allExamples
    , makeNegativeCorpusTest [Seal AmountOfMoney]
      $ withLocale negativeCorpus localeUS US.negativeExamples
    ]
    , testGroup "EN_ZA Tests"
    [ makeCorpusTest [Seal AmountOfMoney]
      $ withLocale corpus localeZA ZA.allExamples
    , makeNegativeCorpusTest [Seal AmountOfMoney]
      $ withLocale negativeCorpus localeZA ZA.negativeExamples
    ]
  ]
  where
    localeAU = makeLocale EN $ Just AU
    localeBZ = makeLocale EN $ Just BZ
    localeCA = makeLocale EN $ Just R.CA
    localeGB = makeLocale EN $ Just GB
    localeIE = makeLocale EN $ Just IE
    localeIN = makeLocale EN $ Just IN
    localeJM = makeLocale EN $ Just JM
    localeNZ = makeLocale EN $ Just NZ
    localePH = makeLocale EN $ Just PH
    localeTT = makeLocale EN $ Just TT
    localeUS = makeLocale EN $ Just US
    localeZA = makeLocale EN $ Just ZA


intersectTests :: TestTree
intersectTests = testCase "Intersect Test" $
  mapM_ (analyzedNTest testContext testOptions . withTargets [Seal AmountOfMoney]) xs
  where
    xs = [ ("7c7", 2)
         , ("7c7c", 3)
         , ("10 dollars 0.17", 2)
         , ("1.1 dollars 6", 2)
         , ("10 cents and 8 cents", 2)
         , ("10 dollars and 2 dollars", 2)
         , ("between 4.1 dollars and 4 dollars", 3)
         , ("between 4 dollars and 7 euros", 2)
         ]

rangeTests :: TestTree
rangeTests = testCase "Range Test" $
  mapM_ (analyzedRangeTest testContext testOptions . withTargets [Seal AmountOfMoney]) xs
  where
    xs = [ ("between 3 and 1 dollars", Range 14 23)
         , ("between 1 and between 2 and 3 dollars", Range 14 37)
         , ("10 cents and 0.1", Range 0 8)
         , ("Pay Kiran1 10eur", Range 11 16)
         ]

latentTests :: TestTree
latentTests = makeCorpusTest [Seal AmountOfMoney] latentCorpus
