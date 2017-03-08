-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


module Duckling.Finance.Tests (tests) where

import Data.String
import Prelude
import Test.Tasty

import qualified Duckling.Finance.EN.Tests as EN
import qualified Duckling.Finance.ES.Tests as ES
import qualified Duckling.Finance.FR.Tests as FR
import qualified Duckling.Finance.GA.Tests as GA
import qualified Duckling.Finance.ID.Tests as ID
import qualified Duckling.Finance.KO.Tests as KO
import qualified Duckling.Finance.NB.Tests as NB
import qualified Duckling.Finance.PT.Tests as PT
import qualified Duckling.Finance.RO.Tests as RO
import qualified Duckling.Finance.SV.Tests as SV
import qualified Duckling.Finance.VI.Tests as VI

tests :: TestTree
tests = testGroup "Finance Tests"
  [ EN.tests
  , ES.tests
  , FR.tests
  , GA.tests
  , ID.tests
  , KO.tests
  , NB.tests
  , PT.tests
  , RO.tests
  , SV.tests
  , VI.tests
  ]
