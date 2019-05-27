-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.Tests (tests) where

import Data.Aeson
import Data.Aeson.Types ((.:), parseMaybe, withObject)
import Data.String
import Data.Text (Text)
import Data.Time
import Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Duckling.Dimensions.Types
import Duckling.Testing.Asserts
import Duckling.Testing.Types
import Duckling.Time.Types
import Duckling.TimeGrain.Types
import qualified Duckling.Time.AR.Tests as AR
import qualified Duckling.Time.DA.Tests as DA
import qualified Duckling.Time.DE.Tests as DE
import qualified Duckling.Time.EN.Tests as EN
import qualified Duckling.Time.EL.Tests as EL
import qualified Duckling.Time.ES.Tests as ES
import qualified Duckling.Time.FR.Tests as FR
import qualified Duckling.Time.GA.Tests as GA
import qualified Duckling.Time.HR.Tests as HR
import qualified Duckling.Time.HE.Tests as HE
import qualified Duckling.Time.HU.Tests as HU
import qualified Duckling.Time.IT.Tests as IT
import qualified Duckling.Time.KA.Tests as KA
import qualified Duckling.Time.KO.Tests as KO
import qualified Duckling.Time.NB.Tests as NB
import qualified Duckling.Time.NL.Tests as NL
import qualified Duckling.Time.PL.Tests as PL
import qualified Duckling.Time.PT.Tests as PT
import qualified Duckling.Time.RO.Tests as RO
import qualified Duckling.Time.SV.Tests as SV
import qualified Duckling.Time.UK.Tests as UK
import qualified Duckling.Time.VI.Tests as VI
import qualified Duckling.Time.ZH.Tests as ZH

tests :: TestTree
tests = testGroup "Time Tests"
  [ AR.tests
  , DA.tests
  , DE.tests
  , EL.tests
  , EN.tests
  , ES.tests
  , FR.tests
  , GA.tests
  , HR.tests
  , HE.tests
  , HU.tests
  , IT.tests
  , KA.tests
  , KO.tests
  , NB.tests
  , NL.tests
  , PL.tests
  , PT.tests
  , RO.tests
  , SV.tests
  , UK.tests
  , VI.tests
  , ZH.tests
  , timeFormatTest
  , timeIntersectTest
  ]

timeFormatTest :: TestTree
timeFormatTest = testCase "Format Test" $
  mapM_ (analyzedFirstTest testContext testOptions . withTargets [This Time]) xs
  where
    xs = examplesCustom (parserCheck expected parseValue) ["now"]
    expected = "2013-02-12T04:30:00.000-02:00"

parseValue :: Value -> Maybe Text
parseValue = parseMaybe . withObject "value object" $ \o -> o .: "value"

timeIntersectTest :: TestTree
timeIntersectTest = testCase "Intersect Test" $ mapM_ check
  [ ((t01, t01), Just t01)
  , ((t01, t12), Nothing)
  , ((t12, t13), Just t12)
  , ((t12, t34), Nothing)
  , ((t13, t23), Just t23)
  , ((t13, t24), Just t23)
  , ((t0_, t0_), Just t0_)
  , ((t0_, t1_), Nothing)
  , ((t0_, t01), Just t01)
  , ((t0_, t13), Nothing)
  , ((t0_, t2_), Nothing)
  , ((t0m, t13), Just t13)

  , ((t12, t01), Nothing)
  , ((t13, t12), Just t12)
  , ((t34, t12), Nothing)
  , ((t23, t13), Just t23)
  , ((t24, t13), Just t23)
  , ((t1_, t0_), Nothing)
  , ((t01, t0_), Just t01)
  , ((t13, t0_), Nothing)
  , ((t2_, t0_), Nothing)
  , ((t13, t0m), Just t13)

  , ((t13, t2m), Just t23)
  , ((t2m, t13), Just t23)
  ]
  where
    check :: ((TimeObject, TimeObject), Maybe TimeObject) -> IO ()
    check ((t1, t2), exp) =
      assertEqual "wrong intersect" exp $ timeIntersect t1 t2
    t0_ = TimeObject t0 Second Nothing
    t0m = TimeObject t0 Minute Nothing
    t01 = TimeObject t0 Second $ Just t1
    t1_ = TimeObject t1 Second Nothing
    t12 = TimeObject t1 Second $ Just t2
    t13 = TimeObject t1 Second $ Just t3
    t2_ = TimeObject t2 Second Nothing
    t2m = TimeObject t2 Minute Nothing
    t23 = TimeObject t2 Second $ Just t3
    t24 = TimeObject t2 Second $ Just t4
    t34 = TimeObject t3 Second $ Just t4
    t0 = UTCTime day 0
    t1 = UTCTime day 1
    t2 = UTCTime day 2
    t3 = UTCTime day 3
    t4 = UTCTime day 4
    day = fromGregorian 2017 2 8
