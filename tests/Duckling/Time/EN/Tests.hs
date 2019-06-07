-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Duckling.Time.EN.Tests
  ( tests
  ) where

import Data.Aeson
import Data.Aeson.Types ((.:), parseMaybe, withObject)
import Data.String
import Data.Text (Text)
import Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Asserts
import Duckling.Testing.Types hiding (examples)
import Duckling.Time.Corpus
import Duckling.Time.EN.Corpus
import Duckling.TimeGrain.Types (Grain(..))
import Duckling.Types (Range(..))
import qualified Duckling.Time.EN.AU.Corpus as AU
import qualified Duckling.Time.EN.BZ.Corpus as BZ
import qualified Duckling.Time.EN.CA.Corpus as CA
import qualified Duckling.Time.EN.GB.Corpus as GB
import qualified Duckling.Time.EN.IE.Corpus as IE
import qualified Duckling.Time.EN.IN.Corpus as IN
import qualified Duckling.Time.EN.JM.Corpus as JM
import qualified Duckling.Time.EN.NZ.Corpus as NZ
import qualified Duckling.Time.EN.PH.Corpus as PH
import qualified Duckling.Time.EN.TT.Corpus as TT
import qualified Duckling.Time.EN.US.Corpus as US
import qualified Duckling.Time.EN.ZA.Corpus as ZA

tests :: TestTree
tests = testGroup "EN Tests"
  [ makeCorpusTest [This Time] defaultCorpus
  , makeNegativeCorpusTest [This Time] negativeCorpus
  , makeCorpusTest [This Time] diffCorpus
  , exactSecondTests
  , valuesTest
  , intersectTests
  , rangeTests
  , localeTests
  , makeCorpusTest [This Time] latentCorpus
  ]

localeTests :: TestTree
localeTests = testGroup "Locale Tests"
  [ testGroup "EN_AU Tests"
    [ makeCorpusTest [This Time] $ withLocale corpus localeAU AU.allExamples
    , makeNegativeCorpusTest [This Time] $ withLocale negativeCorpus localeAU []
    ]
  , testGroup "EN_BZ Tests"
    [ makeCorpusTest [This Time] $ withLocale corpus localeBZ BZ.allExamples
    , makeNegativeCorpusTest [This Time] $ withLocale negativeCorpus localeBZ []
    ]
  , testGroup "EN_CA Tests"
    [ makeCorpusTest [This Time] $ withLocale corpus localeCA CA.allExamples
    , makeNegativeCorpusTest [This Time] $ withLocale negativeCorpus localeCA []
    ]
  , testGroup "EN_GB Tests"
    [ makeCorpusTest [This Time] $ withLocale corpus localeGB GB.allExamples
    , makeNegativeCorpusTest [This Time] $ withLocale negativeCorpus localeGB []
    ]
  , testGroup "EN_IE Tests"
    [ makeCorpusTest [This Time] $ withLocale corpus localeIE IE.allExamples
    , makeNegativeCorpusTest [This Time] $ withLocale negativeCorpus localeIE []
    ]
  , testGroup "EN_IN Tests"
    [ makeCorpusTest [This Time] $ withLocale corpus localeIN IN.allExamples
    , makeNegativeCorpusTest [This Time] $ withLocale negativeCorpus localeIN []
    ]
  , testGroup "EN_JM Tests"
    [ makeCorpusTest [This Time] $ withLocale corpus localeJM JM.allExamples
    , makeNegativeCorpusTest [This Time] $ withLocale negativeCorpus localeJM []
    ]
  , testGroup "EN_NZ Tests"
    [ makeCorpusTest [This Time] $ withLocale corpus localeNZ NZ.allExamples
    , makeNegativeCorpusTest [This Time] $ withLocale negativeCorpus localeNZ []
    ]
  , testGroup "EN_PH Tests"
    [ makeCorpusTest [This Time] $ withLocale corpus localePH PH.allExamples
    , makeNegativeCorpusTest [This Time] $ withLocale negativeCorpus localePH []
    ]
  , testGroup "EN_TT Tests"
    [ makeCorpusTest [This Time] $ withLocale corpus localeTT TT.allExamples
    , makeNegativeCorpusTest [This Time] $ withLocale negativeCorpus localeTT []
    ]
  , testGroup "EN_US Tests"
    [ makeCorpusTest [This Time] $ withLocale corpus localeUS US.allExamples
    , makeNegativeCorpusTest [This Time] $ withLocale negativeCorpus localeUS []
    ]
  , testGroup "EN_ZA Tests"
    [ makeCorpusTest [This Time] $ withLocale corpus localeZA ZA.allExamples
    , makeNegativeCorpusTest [This Time] $ withLocale negativeCorpus localeZA []
    ]
  ]
  where
    localeAU = makeLocale EN $ Just AU
    localeBZ = makeLocale EN $ Just BZ
    localeCA = makeLocale EN $ Just CA
    localeGB = makeLocale EN $ Just GB
    localeIE = makeLocale EN $ Just IE
    localeIN = makeLocale EN $ Just IN
    localeJM = makeLocale EN $ Just JM
    localeNZ = makeLocale EN $ Just NZ
    localePH = makeLocale EN $ Just PH
    localeTT = makeLocale EN $ Just TT
    localeUS = makeLocale EN $ Just US
    localeZA = makeLocale EN $ Just ZA

exactSecondTests :: TestTree
exactSecondTests = testCase "Exact Second Tests" $
  mapM_ (analyzedFirstTest context testOptions . withTargets [This Time]) xs
  where
    context = testContext {referenceTime = refTime (2016, 12, 6, 13, 21, 42) 1}
    xs = concat
      [ examples (datetime (2016, 12, 6, 13, 21, 45) Second)
                 [ "in 3 seconds"
                 ]
      , examples (datetime (2016, 12, 6, 13, 31, 42) Second)
                 [ "in ten minutes"
                 ]
      , examples (datetimeInterval
          ((2016, 12, 6, 13, 21, 42), (2016, 12, 12, 0, 0, 0)) Second)
                 [ "by next week"
                 , "by Monday"
                 ]
      ]

valuesTest :: TestTree
valuesTest = testCase "Values Test" $
  mapM_ (analyzedFirstTest testContext testOptions . withTargets [This Time]) xs
  where
    xs = examplesCustom (parserCheck 1 parseValuesSize)
                        [ "now"
                        , "8 o'clock tonight"
                        , "tonight at 8 o'clock"
                        , "yesterday"
                        ]
    parseValuesSize :: Value -> Maybe Int
    parseValuesSize x = length <$> parseValues x
    parseValues :: Value -> Maybe [Object]
    parseValues = parseMaybe $ withObject "value object" (.: "values")

intersectTests :: TestTree
intersectTests = testCase "Intersect Test" $
  mapM_ (analyzedNTest testContext testOptions . withTargets [This Time]) xs
  where
    xs = [ ("tomorrow July", 2)
         , ("Mar tonight", 2)
         , ("Feb tomorrow", 1) -- we are in February
         ]

rangeTests :: TestTree
rangeTests = testCase "Range Test" $
  mapM_ (analyzedRangeTest testContext testOptions . withTargets [This Time]) xs
  where
    xs = [ ("at 615.", Range 0 6) -- make sure ruleHHMMLatent allows this
         , ("last in 2'", Range 5 10) -- ruleLastTime too eager
         , ("this in 2'", Range 5 10) -- ruleThisTime too eager
         , ("next in 2'", Range 5 10) -- ruleNextTime too eager
         , ("this this week", Range 5 14) -- ruleThisTime too eager
         , ("one ninety nine a m", Range 11 19) -- ruleMilitarySpelledOutAMPM2
         , ("thirteen fifty nine a m", Range 15 23) -- ruleMilitarySpelledOutAMPM
         , ("table Wednesday for 30 people", Range 6 15)
           -- do not parse "for 30" as year intersect
         , ("house 1 on december 2013", Range 11 24) -- ruleAbsorbOnDay
         , ("at 6pm GMT PDT", Range 0 10) -- ruleTimezone
         , ("at 6pm (PDT) GMT", Range 0 12) -- ruleTimezoneBracket
         , ("6pm GMT - 8pm GMT PDT", Range 0 17)
           -- ruleTimezone will not match because TimeData hasTimezone.
         ]
