-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


module Duckling.Quantity.Tests
  ( tests
  ) where

import Data.String
import Prelude
import Test.Tasty

import qualified Duckling.Quantity.AR.Tests as AR
import qualified Duckling.Quantity.EN.Tests as EN
import qualified Duckling.Quantity.FR.Tests as FR
import qualified Duckling.Quantity.HR.Tests as HR
import qualified Duckling.Quantity.KO.Tests as KO
import qualified Duckling.Quantity.PT.Tests as PT
import qualified Duckling.Quantity.RO.Tests as RO
import qualified Duckling.Quantity.RU.Tests as RU
import qualified Duckling.Quantity.ZH.Tests as ZH

tests :: TestTree
tests = testGroup "Quantity Tests"
  [ AR.tests
  , EN.tests
  , FR.tests
  , HR.tests
  , KO.tests
  , PT.tests
  , RO.tests
  , RU.tests
  , ZH.tests
  ]
