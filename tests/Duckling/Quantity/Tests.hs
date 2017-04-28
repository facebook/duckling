-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


module Duckling.Quantity.Tests (tests) where

import Prelude
import Data.String
import Test.Tasty

import qualified Duckling.Quantity.EN.Tests as EN
import qualified Duckling.Quantity.FR.Tests as FR
import qualified Duckling.Quantity.HR.Tests as HR
import qualified Duckling.Quantity.KO.Tests as KO
import qualified Duckling.Quantity.PT.Tests as PT
import qualified Duckling.Quantity.RO.Tests as RO

tests :: TestTree
tests = testGroup "Quantity Tests"
  [ EN.tests
  , FR.tests
  , HR.tests
  , KO.tests
  , PT.tests
  , RO.tests
  ]
