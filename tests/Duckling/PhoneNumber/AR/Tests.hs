-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


module Duckling.PhoneNumber.AR.Tests
  ( tests
  ) where

import Data.String
import Test.Tasty

import Duckling.Dimensions.Types
import Duckling.PhoneNumber.AR.Corpus
import Duckling.Testing.Asserts

tests :: TestTree
tests = testGroup "PhoneNumber Tests"
  [ makeCorpusTest [This PhoneNumber] corpus
  , makeNegativeCorpusTest [This PhoneNumber] negativeCorpus
  ]
