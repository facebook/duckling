-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


module Duckling.Numeral.Tests (tests) where

import Data.String
import Prelude
import Test.Tasty

import qualified Duckling.Numeral.AR.Tests as AR
import qualified Duckling.Numeral.BG.Tests as BG
import qualified Duckling.Numeral.CS.Tests as CS
import qualified Duckling.Numeral.DA.Tests as DA
import qualified Duckling.Numeral.DE.Tests as DE
import qualified Duckling.Numeral.EL.Tests as EL
import qualified Duckling.Numeral.EN.Tests as EN
import qualified Duckling.Numeral.ES.Tests as ES
import qualified Duckling.Numeral.ET.Tests as ET
import qualified Duckling.Numeral.FR.Tests as FR
import qualified Duckling.Numeral.GA.Tests as GA
import qualified Duckling.Numeral.HE.Tests as HE
import qualified Duckling.Numeral.HI.Tests as HI
import qualified Duckling.Numeral.HR.Tests as HR
import qualified Duckling.Numeral.HU.Tests as HU
import qualified Duckling.Numeral.ID.Tests as ID
import qualified Duckling.Numeral.IT.Tests as IT
import qualified Duckling.Numeral.JA.Tests as JA
import qualified Duckling.Numeral.KA.Tests as KA
import qualified Duckling.Numeral.KO.Tests as KO
import qualified Duckling.Numeral.MY.Tests as MY
import qualified Duckling.Numeral.NB.Tests as NB
import qualified Duckling.Numeral.NE.Tests as NE
import qualified Duckling.Numeral.NL.Tests as NL
import qualified Duckling.Numeral.PL.Tests as PL
import qualified Duckling.Numeral.PT.Tests as PT
import qualified Duckling.Numeral.RO.Tests as RO
import qualified Duckling.Numeral.RU.Tests as RU
import qualified Duckling.Numeral.SV.Tests as SV
import qualified Duckling.Numeral.TA.Tests as TA
import qualified Duckling.Numeral.TR.Tests as TR
import qualified Duckling.Numeral.UK.Tests as UK
import qualified Duckling.Numeral.VI.Tests as VI
import qualified Duckling.Numeral.ZH.Tests as ZH

tests :: TestTree
tests = testGroup "Numeral Tests"
  [ AR.tests
  , BG.tests
  , CS.tests
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
  , KA.tests
  , KO.tests
  , MY.tests
  , NB.tests
  , NE.tests
  , NL.tests
  , PL.tests
  , PT.tests
  , RO.tests
  , RU.tests
  , SV.tests
  , TA.tests
  , TR.tests
  , UK.tests
  , VI.tests
  , ZH.tests
  ]
