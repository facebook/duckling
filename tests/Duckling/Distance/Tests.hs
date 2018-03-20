-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


module Duckling.Distance.Tests
  ( tests
  ) where

import Data.String
import Prelude
import Test.Tasty

import qualified Duckling.Distance.BG.Tests as BG
import qualified Duckling.Distance.CS.Tests as CS
import qualified Duckling.Distance.EN.Tests as EN
import qualified Duckling.Distance.BG.Tests as BG
import qualified Duckling.Distance.ES.Tests as ES
import qualified Duckling.Distance.FR.Tests as FR
import qualified Duckling.Distance.GA.Tests as GA
import qualified Duckling.Distance.HR.Tests as HR
import qualified Duckling.Distance.KO.Tests as KO
import qualified Duckling.Distance.NL.Tests as NL
import qualified Duckling.Distance.PT.Tests as PT
import qualified Duckling.Distance.RO.Tests as RO
import qualified Duckling.Distance.RU.Tests as RU
import qualified Duckling.Distance.TR.Tests as TR

tests :: TestTree
tests = testGroup "Distance Tests"
  [ BG.tests
  , CS.tests
  , EN.tests
  , ES.tests
  , FR.tests
  , GA.tests
  , HR.tests
  , KO.tests
  , NL.tests
  , PT.tests
  , RO.tests
  , RU.tests
  , TR.tests
  ]
