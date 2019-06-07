-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


module Duckling.Duration.Tests
  ( tests
  ) where

import Data.String
import Prelude
import Test.Tasty

import qualified Duckling.Duration.AR.Tests as AR
import qualified Duckling.Duration.BG.Tests as BG
import qualified Duckling.Duration.EL.Tests as EL
import qualified Duckling.Duration.EN.Tests as EN
import qualified Duckling.Duration.FR.Tests as FR
import qualified Duckling.Duration.GA.Tests as GA
import qualified Duckling.Duration.HI.Tests as HI
import qualified Duckling.Duration.HU.Tests as HU
import qualified Duckling.Duration.JA.Tests as JA
import qualified Duckling.Duration.KA.Tests as KA
import qualified Duckling.Duration.KO.Tests as KO
import qualified Duckling.Duration.MN.Tests as MN
import qualified Duckling.Duration.NB.Tests as NB
import qualified Duckling.Duration.NL.Tests as NL
import qualified Duckling.Duration.PL.Tests as PL
import qualified Duckling.Duration.PT.Tests as PT
import qualified Duckling.Duration.RO.Tests as RO
import qualified Duckling.Duration.RU.Tests as RU
import qualified Duckling.Duration.SV.Tests as SV
import qualified Duckling.Duration.TR.Tests as TR
import qualified Duckling.Duration.UK.Tests as UK
import qualified Duckling.Duration.ZH.Tests as ZH

tests :: TestTree
tests = testGroup "Duration Tests"
  [ AR.tests
  , BG.tests
  , EL.tests
  , EN.tests
  , FR.tests
  , GA.tests
  , HI.tests
  , HU.tests
  , JA.tests
  , KA.tests
  , KO.tests
  , MN.tests
  , NB.tests
  , NL.tests
  , PL.tests
  , PT.tests
  , RO.tests
  , RU.tests
  , SV.tests
  , TR.tests
  , UK.tests
  , ZH.tests
  ]
