-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


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

tests :: TestTree
tests = testGroup "Engine Tests"
  [ emptyRegexTest
  ]

emptyRegexTest :: TestTree
emptyRegexTest = testCase "Empty Regex Test" $
  case regex "()" of
    Regex regex -> assertEqual "empty result" [] $
      runDuckling $ lookupRegex regex 0 "hey"
    _ -> assertFailure "expected a regex"
