-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


module Duckling.Temperature.Tests (tests) where

import Prelude
import Data.String
import Test.Tasty

import qualified Duckling.Temperature.AR.Tests as AR
import qualified Duckling.Temperature.EN.Tests as EN
import qualified Duckling.Temperature.ES.Tests as ES
import qualified Duckling.Temperature.FR.Tests as FR
import qualified Duckling.Temperature.GA.Tests as GA
import qualified Duckling.Temperature.HI.Tests as HI
import qualified Duckling.Temperature.HR.Tests as HR
import qualified Duckling.Temperature.IT.Tests as IT
import qualified Duckling.Temperature.JA.Tests as JA
import qualified Duckling.Temperature.KM.Tests as KM
import qualified Duckling.Temperature.KO.Tests as KO
import qualified Duckling.Temperature.MN.Tests as MN
import qualified Duckling.Temperature.PT.Tests as PT
import qualified Duckling.Temperature.RO.Tests as RO
import qualified Duckling.Temperature.TR.Tests as TR
import qualified Duckling.Temperature.ZH.Tests as ZH

tests :: TestTree
tests = testGroup "Temperature Tests"
  [ AR.tests
  , EN.tests
  , ES.tests
  , FR.tests
  , GA.tests
  , HI.tests
  , HR.tests
  , IT.tests
  , JA.tests
  , KM.tests
  , KO.tests
  , MN.tests
  , PT.tests
  , RO.tests
  , TR.tests
  , ZH.tests
  ]
