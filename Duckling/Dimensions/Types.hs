-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}

module Duckling.Dimensions.Types
  ( Some(..)
  , Dimension(..)
  , dimEq
  ) where

import Data.Hashable
import Data.Maybe
-- Intentionally limit use of Typeable to avoid casting or typeOf usage
import Data.Typeable ((:~:)(..))
import TextShow (TextShow(..))
import qualified TextShow as TS
import Prelude

import Duckling.Distance.Types (DistanceData)
import Duckling.Duration.Types (DurationData)
import Duckling.Email.Types (EmailData)
import Duckling.Finance.Types (FinanceData)
import Duckling.Number.Types (NumberData)
import Duckling.Ordinal.Types (OrdinalData)
import Duckling.PhoneNumber.Types (PhoneNumberData)
import Duckling.Quantity.Types (QuantityData)
import Duckling.Regex.Types (GroupMatch)
import Duckling.Temperature.Types (TemperatureData)
import Duckling.Time.Types (TimeData)
import Duckling.TimeGrain.Types (Grain)
import Duckling.Url.Types (UrlData)
import Duckling.Volume.Types (VolumeData)

-- -----------------------------------------------------------------
-- Wrapper to house the existential

-- TODO: get rid of this t14593551
data Some t = forall a . Some (t a)

-- -----------------------------------------------------------------
-- Dimension

-- | GADT for differentiating between dimensions
-- Each dimension should have its own constructor and provide the data structure
-- for its parsed data
data Dimension a where
  RegexMatch :: Dimension GroupMatch
  Distance :: Dimension DistanceData
  Duration :: Dimension DurationData
  Email :: Dimension EmailData
  Finance :: Dimension FinanceData
  DNumber :: Dimension NumberData
  Ordinal :: Dimension OrdinalData
  PhoneNumber :: Dimension PhoneNumberData
  Quantity :: Dimension QuantityData
  Temperature :: Dimension TemperatureData
  Time :: Dimension TimeData
  TimeGrain :: Dimension Grain
  Url :: Dimension UrlData
  Volume :: Dimension VolumeData

-- Show
deriving instance Show (Some Dimension)
instance Show (Dimension a) where
  show RegexMatch = "regex"
  show Distance = "distance"
  show Duration = "duration"
  show Email = "email"
  show Finance = "amount-of-money"
  show DNumber = "number"
  show Ordinal = "ordinal"
  show PhoneNumber = "phone-number"
  show Quantity = "quantity"
  show Temperature = "temperature"
  show Time = "time"
  show TimeGrain = "time-grain"
  show Url = "url"
  show Volume = "volume"

-- TextShow
instance TextShow (Dimension a) where
  showb d = TS.fromString $ show d
instance TextShow (Some Dimension) where
  showb (Some d) = showb d

-- Eq
deriving instance Eq (Dimension a)
instance Eq (Some Dimension) where
  (==) (Some a) (Some b) = isJust $ dimEq a b

-- Hashable
instance Hashable (Some Dimension) where
  hashWithSalt s (Some a) = hashWithSalt s a
instance Hashable (Dimension a) where
  hashWithSalt s RegexMatch  = hashWithSalt s (0::Int)
  hashWithSalt s Distance    = hashWithSalt s (1::Int)
  hashWithSalt s Duration    = hashWithSalt s (2::Int)
  hashWithSalt s Email       = hashWithSalt s (3::Int)
  hashWithSalt s Finance     = hashWithSalt s (4::Int)
  hashWithSalt s DNumber     = hashWithSalt s (5::Int)
  hashWithSalt s Ordinal     = hashWithSalt s (6::Int)
  hashWithSalt s PhoneNumber = hashWithSalt s (7::Int)
  hashWithSalt s Quantity    = hashWithSalt s (8::Int)
  hashWithSalt s Temperature = hashWithSalt s (9::Int)
  hashWithSalt s Time        = hashWithSalt s (10::Int)
  hashWithSalt s TimeGrain   = hashWithSalt s (11::Int)
  hashWithSalt s Url         = hashWithSalt s (12::Int)
  hashWithSalt s Volume      = hashWithSalt s (13::Int)

instance Read (Some Dimension) where
  -- Regex is intentionally ignored
  readsPrec _ "amount-of-money" = [(Some Finance, "")]
  readsPrec _ "distance" = [(Some Distance, "")]
  readsPrec _ "duration" = [(Some Duration, "")]
  readsPrec _ "email" = [(Some Email, "")]
  readsPrec _ "number" = [(Some DNumber, "")]
  readsPrec _ "ordinal" = [(Some Ordinal, "")]
  readsPrec _ "phone-number" = [(Some PhoneNumber, "")]
  readsPrec _ "quantity" = [(Some Quantity, "")]
  readsPrec _ "temperature" = [(Some Temperature, "")]
  readsPrec _ "time" = [(Some Time, "")]
  readsPrec _ "url" = [(Some Url, "")]
  readsPrec _ "volume" = [(Some Volume, "")]
  readsPrec _ _ = []

-- | Proof that 2 dimensions are the same Type
-- 2 matches per dimension for pattern exhaustiveness sake
dimEq :: Dimension a -> Dimension b -> Maybe (a :~: b)
dimEq RegexMatch RegexMatch = Just Refl
dimEq RegexMatch _ = Nothing
dimEq Distance Distance = Just Refl
dimEq Distance _ = Nothing
dimEq Duration Duration = Just Refl
dimEq Duration _ = Nothing
dimEq Email Email = Just Refl
dimEq Email _ = Nothing
dimEq Finance Finance = Just Refl
dimEq Finance _ = Nothing
dimEq DNumber DNumber = Just Refl
dimEq DNumber _ = Nothing
dimEq Ordinal Ordinal = Just Refl
dimEq Ordinal _ = Nothing
dimEq PhoneNumber PhoneNumber = Just Refl
dimEq PhoneNumber _ = Nothing
dimEq Quantity Quantity = Just Refl
dimEq Quantity _ = Nothing
dimEq Temperature Temperature = Just Refl
dimEq Temperature _ = Nothing
dimEq Time Time = Just Refl
dimEq Time _ = Nothing
dimEq TimeGrain TimeGrain = Just Refl
dimEq TimeGrain _ = Nothing
dimEq Url Url = Just Refl
dimEq Url _ = Nothing
dimEq Volume Volume = Just Refl
dimEq Volume _ = Nothing
