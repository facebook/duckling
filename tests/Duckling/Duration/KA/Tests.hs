-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


module Duckling.Duration.KA.Tests
  ( tests
  ) where

import Prelude
import Data.String
import Test.Tasty

import Duckling.Dimensions.Types
import Duckling.Duration.KA.Corpus
import Duckling.Testing.Asserts

tests :: TestTree
tests = testGroup "KA Tests"
  [ makeCorpusTest [This Duration] corpus
  ]
