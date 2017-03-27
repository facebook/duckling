-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


module Duckling.Duration.ZH.Tests
  ( tests
  ) where

import Prelude
import Data.String
import Test.Tasty
import Test.Tasty.HUnit

import Duckling.Dimensions.Types
import Duckling.Duration.ZH.Corpus
import Duckling.Lang
import Duckling.Resolve
import Duckling.Testing.Asserts

tests :: TestTree
tests = testGroup "ZH Tests"
  [ testCase "Corpus Tests" $
      mapM_ (analyzedFirstTest context {lang = ZH} . withTargets [This Duration])
        xs
  ]
  where
    (context, xs) = corpus
