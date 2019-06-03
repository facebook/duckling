-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


module Duckling.Volume.Tests
  ( tests
  ) where

import Data.String
import Prelude
import Test.Tasty

import qualified Duckling.Volume.AR.Tests as AR
import qualified Duckling.Volume.DE.Tests as DE
import qualified Duckling.Volume.EN.Tests as EN
import qualified Duckling.Volume.ES.Tests as ES
import qualified Duckling.Volume.FR.Tests as FR
import qualified Duckling.Volume.GA.Tests as GA
import qualified Duckling.Volume.HR.Tests as HR
import qualified Duckling.Volume.IT.Tests as IT
import qualified Duckling.Volume.KM.Tests as KM
import qualified Duckling.Volume.KO.Tests as KO
import qualified Duckling.Volume.MN.Tests as MN
import qualified Duckling.Volume.NL.Tests as NL
import qualified Duckling.Volume.PT.Tests as PT
import qualified Duckling.Volume.RO.Tests as RO
import qualified Duckling.Volume.RU.Tests as RU
import qualified Duckling.Volume.TR.Tests as TR

tests :: TestTree
tests = testGroup "Volume Tests"
  [ AR.tests
  , DE.tests
  , EN.tests
  , ES.tests
  , FR.tests
  , GA.tests
  , HR.tests
  , IT.tests
  , KM.tests
  , KO.tests
  , MN.tests
  , NL.tests
  , PT.tests
  , RO.tests
  , RU.tests
  , TR.tests
  ]
