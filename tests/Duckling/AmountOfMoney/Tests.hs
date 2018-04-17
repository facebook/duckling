-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


module Duckling.AmountOfMoney.Tests
  ( tests
  ) where

import Data.String
import Prelude
import Test.Tasty

import qualified Duckling.AmountOfMoney.AR.Tests as AR
import qualified Duckling.AmountOfMoney.EN.Tests as EN
import qualified Duckling.AmountOfMoney.BG.Tests as BG
import qualified Duckling.AmountOfMoney.ES.Tests as ES
import qualified Duckling.AmountOfMoney.FR.Tests as FR
import qualified Duckling.AmountOfMoney.GA.Tests as GA
import qualified Duckling.AmountOfMoney.HR.Tests as HR
import qualified Duckling.AmountOfMoney.ID.Tests as ID
import qualified Duckling.AmountOfMoney.KO.Tests as KO
import qualified Duckling.AmountOfMoney.NB.Tests as NB
import qualified Duckling.AmountOfMoney.NL.Tests as NL
import qualified Duckling.AmountOfMoney.PT.Tests as PT
import qualified Duckling.AmountOfMoney.RO.Tests as RO
import qualified Duckling.AmountOfMoney.RU.Tests as RU
import qualified Duckling.AmountOfMoney.SV.Tests as SV
import qualified Duckling.AmountOfMoney.VI.Tests as VI
import qualified Duckling.AmountOfMoney.ZH.Tests as ZH

tests :: TestTree
tests = testGroup "AmountOfMoney Tests"
  [ AR.tests
  , EN.tests
  , BG.tests
  , ES.tests
  , FR.tests
  , GA.tests
  , HR.tests
  , ID.tests
  , KO.tests
  , NB.tests
  , NL.tests
  , PT.tests
  , RO.tests
  , RU.tests
  , SV.tests
  , VI.tests
  , ZH.tests
  ]
