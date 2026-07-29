-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


module Duckling.Time.GU.Tests
  ( tests
  ) where

import Prelude
import Test.Tasty

import Duckling.Dimensions.Types
import Duckling.Testing.Asserts
import Duckling.Time.GU.Corpus

tests :: TestTree
tests = testGroup "GU Tests"
  [ makeCorpusTest [Seal Time] corpus
  ]
