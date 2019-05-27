-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE TupleSections #-}

module Duckling.Testing.Types
  ( Corpus
  , Datetime
  , Example
  , NegativeCorpus
  , TestPredicate
  , dt
  , examples
  , examplesCustom
  , parserCheck
  , refTime
  , simpleCheck
  , testContext
  , testOptions
  , withLocale
  , zTime
  ) where

import Data.Aeson (toJSON, ToJSON, Value)
import Data.Fixed (Pico)
import Data.Text (Text)
import Prelude
import qualified Data.Time as Time

import Duckling.Core
import Duckling.Resolve
import Duckling.Types

type TestPredicate = Context -> ResolvedToken -> Bool
type Example = (Text, TestPredicate)
type Corpus = (Context, Options, [Example])
type NegativeCorpus = (Context, Options, [Text])

examplesCustom :: TestPredicate -> [Text] -> [Example]
examplesCustom check = map (, check)

simpleCheck :: ToJSON a => a -> TestPredicate
simpleCheck json _ Resolved{rval = RVal _ v} = toJSON json == toJSON v

parserCheck :: Eq a => a -> (Value -> Maybe a) -> TestPredicate
parserCheck expected parse _ Resolved{rval = RVal _ v} =
  maybe False (expected ==) $ parse (toJSON v)

examples :: ToJSON a => a -> [Text] -> [Example]
examples output = examplesCustom (simpleCheck output)

type Datetime = (Integer, Int, Int, Int, Int, Pico)

dt :: Datetime -> Time.UTCTime
dt (year, month, days, hours, minutes, seconds) = Time.UTCTime day diffTime
  where
    day = Time.fromGregorian year month days
    diffTime = Time.timeOfDayToTime $ Time.TimeOfDay hours minutes seconds

zTime :: Datetime -> Int -> Time.ZonedTime
zTime datetime offset = fromUTC (dt datetime) $ Time.hoursToTimeZone offset

refTime :: Datetime -> Int -> DucklingTime
refTime datetime offset = fromZonedTime $ zTime datetime offset

-- Tuesday Feb 12, 2013 at 4:30am is the "now" for the tests
testContext :: Context
testContext = Context
  { locale = makeLocale EN Nothing
  , referenceTime = refTime (2013, 2, 12, 4, 30, 0) (-2)
  }

testOptions :: Options
testOptions = Options
  { withLatent = False
  }

withLocale :: (Context, Options, [a]) -> Locale -> [a]
  -> (Context, Options, [a])
withLocale (langContext, options, langXs) locale localeXs
  = (langContext {locale = locale}, options, langXs ++ localeXs)
