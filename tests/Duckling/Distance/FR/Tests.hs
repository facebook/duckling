-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


module Duckling.Distance.FR.Tests
  ( tests ) where
import Data.String
import Test.Tasty

import Duckling.Dimensions.Types
import Duckling.Testing.Asserts
import Duckling.Distance.FR.Corpus

tests :: TestTree
tests = testGroup "FR Tests"
  [ makeCorpusTest [This Distance] corpus
  ]
