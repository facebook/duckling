-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.EN.Tests
  ( tests
  ) where

import Data.String
import Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Duckling.Dimensions.Types
import Duckling.Numeral.EN.Corpus
import Duckling.Numeral.Types
import Duckling.Testing.Asserts
import Duckling.Testing.Types
import Duckling.Types (Range(..))

tests :: TestTree
tests = testGroup "EN Tests"
  [ makeCorpusTest [This Numeral] corpus
  , surroundTests
  , rangeTests
  ]

surroundTests :: TestTree
surroundTests = testCase "Surround Tests" $
  mapM_ (analyzedFirstTest testContext . withTargets [This Numeral]) xs
  where
    xs = concat
      [ examples (NumeralValue 3)
                 [ "3km"
                 ]
      , examples (NumeralValue 100000)
                 [ "100k€"
                 , "100k\x20ac"
                 ]
      , examples (NumeralValue 10.99)
                 [ "10.99$"
                 ]
      ]

rangeTests :: TestTree
rangeTests = testCase "Range Test" $
  mapM_ (analyzedRangeTest testContext . withTargets [This Numeral]) xs
  where
    xs = [ ("negative negative 5", Range 9 19) -- prevent double negatives
         , ("negative-5", Range 8 10) -- prevent double negatives
         , ("- -5", Range 2 4) -- prevent clash with engine tokenizer
         ]
