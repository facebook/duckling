-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Tests
  ( tests ) where

import Data.String
import Prelude
import Test.Tasty
import Test.Tasty.HUnit

import qualified Duckling.Api.Tests as Api
import qualified Duckling.Dimensions.Tests as Dimensions
import qualified Duckling.Engine.Tests as Engine

tests :: TestTree
tests = testGroup "Duckling Tests"
  [ Api.tests
  , Dimensions.tests
  , Engine.tests
  ]
