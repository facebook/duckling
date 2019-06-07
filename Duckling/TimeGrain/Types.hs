-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}

module Duckling.TimeGrain.Types
  ( Grain(..)
  , add
  , inSeconds
  , lower
  )
  where

import Control.DeepSeq
import Data.Aeson
import Data.Hashable
import Data.Text.Lazy.Builder (fromText)
import GHC.Generics
import Prelude
import TextShow
import qualified Data.Text as Text
import qualified Data.Time as Time

import Duckling.Resolve (Resolve(..))

data Grain
  -- NoGrain is helpful to define "now"
  = NoGrain | Second | Minute | Hour | Day | Week | Month | Quarter | Year
  deriving (Eq, Generic, Hashable, Ord, Bounded, Enum, Show, NFData)

instance Resolve Grain where
  type ResolvedValue Grain = Grain
  resolve _ _ _ = Nothing

instance TextShow Grain where
  showb = fromText . Text.toLower . Text.pack . show

instance ToJSON Grain where
  toJSON = String . showt

updateUTCDay :: Time.UTCTime -> (Time.Day -> Time.Day) -> Time.UTCTime
updateUTCDay (Time.UTCTime day diffTime) f = Time.UTCTime (f day) diffTime

add :: Time.UTCTime -> Grain -> Integer -> Time.UTCTime
add utcTime NoGrain n = Time.addUTCTime (realToFrac n) utcTime
add utcTime Second n = Time.addUTCTime (realToFrac n) utcTime
add utcTime Minute n = Time.addUTCTime (realToFrac $ 60 * n) utcTime
add utcTime Hour n = Time.addUTCTime (realToFrac $ 3600 * n) utcTime
add utcTime Day n = updateUTCDay utcTime $ Time.addDays n
add utcTime Week n = updateUTCDay utcTime . Time.addDays $ 7 * n
add utcTime Month n = updateUTCDay utcTime $ Time.addGregorianMonthsClip n
add utcTime Quarter n =
  updateUTCDay utcTime . Time.addGregorianMonthsClip $ 3 * n
add utcTime Year n = updateUTCDay utcTime $ Time.addGregorianYearsClip n

inSeconds :: Num a => Grain -> a -> a
inSeconds NoGrain n = n
inSeconds Second  n = n
inSeconds Minute  n = n * 60
inSeconds Hour    n = n * inSeconds Minute 60
inSeconds Day     n = n * inSeconds Hour 24
inSeconds Week    n = n * inSeconds Day 7
inSeconds Month   n = n * inSeconds Day 30
inSeconds Quarter n = n * inSeconds Month 3
inSeconds Year    n = n * inSeconds Day 365

lower :: Grain -> Grain
lower NoGrain = Second
lower Second = Second
lower Year = Month
lower Month = Day
lower x = pred x
