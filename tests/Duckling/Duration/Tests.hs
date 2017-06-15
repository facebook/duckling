-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


module Duckling.Duration.Tests
  ( tests
  ) where

import Prelude
import Data.String
import Test.Tasty

import qualified Duckling.Duration.EN.Tests as EN
import qualified Duckling.Duration.FR.Tests as FR
import qualified Duckling.Duration.GA.Tests as GA
import qualified Duckling.Duration.JA.Tests as JA
import qualified Duckling.Duration.KO.Tests as KO
import qualified Duckling.Duration.NB.Tests as NB
import qualified Duckling.Duration.PL.Tests as PL
import qualified Duckling.Duration.PT.Tests as PT
import qualified Duckling.Duration.RO.Tests as RO
import qualified Duckling.Duration.SV.Tests as SV
import qualified Duckling.Duration.TR.Tests as TR
import qualified Duckling.Duration.ZH.Tests as ZH

tests :: TestTree
tests = testGroup "Duration Tests"
  [ EN.tests
  , FR.tests
  , GA.tests
  , JA.tests
  , KO.tests
  , NB.tests
  , PL.tests
  , PT.tests
  , RO.tests
  , SV.tests
  , TR.tests
  , ZH.tests
  ]
