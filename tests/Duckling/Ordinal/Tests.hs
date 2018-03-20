-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


module Duckling.Ordinal.Tests (tests) where

import Data.String
import Prelude
import Test.Tasty

import qualified Duckling.Ordinal.AR.Tests as AR
import qualified Duckling.Ordinal.BG.Tests as BG
import qualified Duckling.Ordinal.DA.Tests as DA
import qualified Duckling.Ordinal.DE.Tests as DE
import qualified Duckling.Ordinal.EL.Tests as EL
import qualified Duckling.Ordinal.EN.Tests as EN
import qualified Duckling.Ordinal.ES.Tests as ES
import qualified Duckling.Ordinal.ET.Tests as ET
import qualified Duckling.Ordinal.FR.Tests as FR
import qualified Duckling.Ordinal.GA.Tests as GA
import qualified Duckling.Ordinal.HE.Tests as HE
import qualified Duckling.Ordinal.HI.Tests as HI
import qualified Duckling.Ordinal.HR.Tests as HR
import qualified Duckling.Ordinal.HU.Tests as HU
import qualified Duckling.Ordinal.ID.Tests as ID
import qualified Duckling.Ordinal.IT.Tests as IT
import qualified Duckling.Ordinal.JA.Tests as JA
import qualified Duckling.Ordinal.KO.Tests as KO
import qualified Duckling.Ordinal.NB.Tests as NB
import qualified Duckling.Ordinal.NL.Tests as NL
import qualified Duckling.Ordinal.PL.Tests as PL
import qualified Duckling.Ordinal.PT.Tests as PT
import qualified Duckling.Ordinal.RO.Tests as RO
import qualified Duckling.Ordinal.RU.Tests as RU
import qualified Duckling.Ordinal.SV.Tests as SV
import qualified Duckling.Ordinal.TR.Tests as TR
import qualified Duckling.Ordinal.UK.Tests as UK
import qualified Duckling.Ordinal.VI.Tests as VI
import qualified Duckling.Ordinal.ZH.Tests as ZH

tests :: TestTree
tests = testGroup "Ordinal Tests"
  [ AR.tests
  , BG.tests
  , DA.tests
  , DE.tests
  , EL.tests
  , EN.tests
  , ES.tests
  , ET.tests
  , FR.tests
  , GA.tests
  , HE.tests
  , HI.tests
  , HR.tests
  , HU.tests
  , ID.tests
  , IT.tests
  , JA.tests
  , KO.tests
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
