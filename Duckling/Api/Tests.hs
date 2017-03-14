-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Duckling.Api.Tests (tests) where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.List (sortOn)
import Data.Text (Text)
import Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Duckling.Api
import Duckling.Dimensions.Types
import Duckling.Lang
import qualified Duckling.Number.Types as TNumber
import Duckling.Testing.Asserts
import Duckling.Testing.Types
import Duckling.Types

tests :: TestTree
tests = testGroup "API Tests"
  [ parseTest
  , rankTest
  , rangeTest
  , supportedDimensionsTest
  ]

parseTest :: TestTree
parseTest = testCase "Parse Test" $
  case parse sentence testContext [Some Numeral] of
    [] -> assertFailure "empty result"
    (Entity dim body value start end:_) -> do
      assertEqual "dim" "number" dim
      assertEqual "body" "42" body
      assertEqual "value" val value
      assertEqual "start" 4 start
      assertEqual "end" 6 end
  where
    sentence = "hey 42 there"
    val = toJText TNumber.NumberValue {TNumber.vValue = 42.0}

rankTest :: TestTree
rankTest = testGroup "Rank Tests"
  [ rankFilterTest
  , rankOrderTest
  ]

rankFilterTest :: TestTree
rankFilterTest = testCase "Rank Filter Tests" $ do
  mapM_ check
    [ ( "in 2 minutes"
      , [Some Numeral, Some Duration, Some Time]
      , [Some Time]
      )
    , ( "in 2 minutes, about 42 degrees"
      , [Some Numeral, Some Temperature, Some Time]
      , [Some Time, Some Temperature]
      )
    , ( "today works... and tomorrow at 9pm too"
      , [Some Numeral, Some Time]
      , [Some Time, Some Time]
      )
    , ( "between 9:30 and 11:00 on thursday or Saturday and Thanksgiving Day"
      , [Some Numeral, Some Time]
      , [Some Time, Some Time, Some Time]
      )
    , ("the day after tomorrow 5pm", [Some Time], [Some Time])
    , ("the day after tomorrow 5pm", [Some Time, Some Numeral], [Some Time])
    , ("the day after tomorrow 5pm", [], [Some Time])
    ]
  where
    check :: (Text, [Some Dimension], [Some Dimension]) -> IO ()
    check (sentence, targets, expected) =
      let go = analyze sentence testContext $ HashSet.fromList targets
          actual = flip map go $
                     \(Resolved{node=Node{token=Token d _}}) -> Some d
      in assertEqual ("wrong winners for " ++ show sentence) expected actual

rankOrderTest :: TestTree
rankOrderTest = testCase "Rank Order Tests" $ do
  mapM_ check
    [ ("tomorrow at 5PM or 8PM", [Some Time])
    , ("321 12 3456 ... 7", [Some Numeral])
    , ("42 today 23 tomorrow", [Some Numeral, Some Time])
    ]
  where
    check (s, targets) =
      let tokens = analyze s testContext $ HashSet.fromList targets
        in assertEqual "wrong ordering" (sortOn range tokens) tokens

rangeTest :: TestTree
rangeTest = testCase "Range Tests" $ do
  mapM_ (analyzedFirstTest testContext) xs
  where
    xs = map (\(input, targets, range) -> (input, targets, f range))
             [ ( "order status 3233763377", [Some PhoneNumber], Range 13 23 )
             , ( "  3233763377  "         , [Some PhoneNumber], Range  2 12 )
             , ( " -3233763377"           , [Some PhoneNumber], Range  2 12 )
             , ( "  now"                  , [Some Time]       , Range  2  5 )
             , ( "   Monday  "            , [Some Time]       , Range  3  9 )
             , ( "  next   week "         , [Some Time]       , Range  2 13 )
             , ( "   42\n\n"              , [Some Numeral]    , Range  3  5 )
             ]
    f :: Range -> TestPredicate
    f expected _ (Resolved {range = actual}) = expected == actual

supportedDimensionsTest :: TestTree
supportedDimensionsTest = testCase "Supported Dimensions Test" $ do
  mapM_ check
    [ ( AR
      , [ Some Email, Some Finance, Some PhoneNumber, Some Url, Some Numeral
        , Some Ordinal
        ]
      )
    , ( PL
      , [ Some Email, Some Finance, Some PhoneNumber, Some Url, Some Duration
        , Some Numeral, Some Ordinal, Some Time
        ]
      )
    ]
  where
    check :: (Lang, [Some Dimension]) -> IO ()
    check (l, expected) = case HashMap.lookup l supportedDimensions of
      Nothing -> assertFailure $ "no dimensions for " ++ show l
      Just actual ->
        assertEqual ("wrong dimensions for " ++ show l) expected actual
