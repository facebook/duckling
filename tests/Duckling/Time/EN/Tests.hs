-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


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
import qualified Duckling.Time.EN.GB.Corpus as GB
import qualified Duckling.Time.EN.US.Corpus as US

tests :: TestTree
tests = testGroup "EN Tests"
  [ makeCorpusTest [This Time] defaultCorpus
  , makeNegativeCorpusTest [This Time] negativeCorpus
  , exactSecondTests
  , valuesTest
  , intersectTests
  , localeTests
  ]

localeTests :: TestTree
localeTests = testGroup "Locale Tests"
  [ testGroup "EN_GB Tests"
    [ makeCorpusTest [This Time] $ withLocale corpus localeGB GB.allExamples
    , makeNegativeCorpusTest [This Time] $ withLocale negativeCorpus localeGB []
    ]
  , testGroup "EN_US Tests"
    [ makeCorpusTest [This Time] $ withLocale corpus localeUS US.allExamples
    , makeNegativeCorpusTest [This Time] $ withLocale negativeCorpus localeUS []
    ]
  ]
  where
    localeGB = makeLocale EN $ Just GB
    localeUS = makeLocale EN $ Just US

exactSecondTests :: TestTree
exactSecondTests = testCase "Exact Second Tests" $
  mapM_ (analyzedFirstTest context . withTargets [This Time]) xs
  where
    context = testContext {referenceTime = refTime (2016, 12, 6, 13, 21, 42) 1}
    xs = concat
      [ examples (datetime (2016, 12, 6, 13, 21, 45) Second)
                 [ "in 3 seconds"
                 ]
      , examples (datetime (2016, 12, 6, 13, 31, 42) Second)
                 [ "in ten minutes"
                 ]
      , examples (datetimeInterval ((2016, 12, 6, 13, 21, 42), (2016, 12, 12, 0, 0, 0)) Second)
                 [ "by next week"
                 , "by Monday"
                 ]
      ]

valuesTest :: TestTree
valuesTest = testCase "Values Test" $
  mapM_ (analyzedFirstTest testContext . withTargets [This Time]) xs
  where
    xs = examplesCustom (parserCheck 1 parseValuesSize)
                        [ "now"
                        , "8 o'clock tonight"
                        , "tonight at 8 o'clock"
                        ]
    parseValuesSize :: Value -> Maybe Int
    parseValuesSize x = length <$> parseValues x
    parseValues :: Value -> Maybe [Object]
    parseValues = parseMaybe $ withObject "value object" (.: "values")

intersectTests :: TestTree
intersectTests = testCase "Intersect Test" $
  mapM_ (analyzedNTest testContext . withTargets [This Time]) xs
  where
    xs = [ ("tomorrow July", 2)
         , ("Mar tonight", 2)
         , ("Feb tomorrow", 1) -- we are in February
         ]
