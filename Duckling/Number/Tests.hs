-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


module Duckling.Number.Tests (tests) where

import Data.String
import Prelude
import Test.Tasty

import qualified Duckling.Number.AR.Tests as AR
import qualified Duckling.Number.DA.Tests as DA
import qualified Duckling.Number.DE.Tests as DE
import qualified Duckling.Number.EN.Tests as EN
import qualified Duckling.Number.ES.Tests as ES
import qualified Duckling.Number.ET.Tests as ET
import qualified Duckling.Number.FR.Tests as FR
import qualified Duckling.Number.GA.Tests as GA
import qualified Duckling.Number.ID.Tests as ID
import qualified Duckling.Number.IT.Tests as IT
import qualified Duckling.Number.JA.Tests as JA
import qualified Duckling.Number.KO.Tests as KO
import qualified Duckling.Number.MY.Tests as MY
import qualified Duckling.Number.NB.Tests as NB
import qualified Duckling.Number.NL.Tests as NL
import qualified Duckling.Number.PL.Tests as PL
import qualified Duckling.Number.PT.Tests as PT
import qualified Duckling.Number.RO.Tests as RO
import qualified Duckling.Number.RU.Tests as RU
import qualified Duckling.Number.SV.Tests as SV
import qualified Duckling.Number.TR.Tests as TR
import qualified Duckling.Number.UK.Tests as UK
import qualified Duckling.Number.VI.Tests as VI
import qualified Duckling.Number.ZH.Tests as ZH

tests :: TestTree
tests = testGroup "Number Tests"
  [ AR.tests
  , DA.tests
  , DE.tests
  , EN.tests
  , ES.tests
  , ET.tests
  , FR.tests
  , GA.tests
  , ID.tests
  , IT.tests
  , JA.tests
  , KO.tests
  , MY.tests
  , NB.tests
  , NL.tests
  , PL.tests
  , PT.tests
  , RO.tests
  , RU.tests
  , SV.tests
  , TR.tests
  , UK.tests
  , VI.tests
  , ZH.tests
  ]
