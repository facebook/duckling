-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Engine.Tests
  ( tests
  ) where

import Data.String
import Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Duckling.Engine
import Duckling.Types
import Duckling.Dimensions.Types
import Duckling.Regex.Types

tests :: TestTree
tests = testGroup "Engine Tests"
  [ emptyRegexTest
  , unicodeAndRegexTest
  ]

emptyRegexTest :: TestTree
emptyRegexTest = testCase "Empty Regex Test" $
  case regex "()" of
    Regex regex -> assertEqual "empty result" [] $
      runDuckling $ lookupRegexAnywhere "hey" regex
    _ -> assertFailure "expected a regex"

unicodeAndRegexTest :: TestTree
unicodeAndRegexTest = testCase "Unicode and Regex Test" $
  case regex "\\$([0-9]*)" of
    Regex regex -> do --
      assertEqual "" expected $
        runDuckling $ lookupRegexAnywhere "\128526 $35" regex
    _ -> assertFailure "expected a regex"
  where
  expected =
    [ Node
      { nodeRange = Range 2 5
      , token = Token RegexMatch (GroupMatch ["35"])
      , children = []
      , rule = Nothing
      }
    ]
