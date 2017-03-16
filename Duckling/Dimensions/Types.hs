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


module Duckling.Dimensions.Types
  ( Some(..)
  , Dimension(..)

  , fromName
  , toName
  ) where

import Data.GADT.Compare
import Data.GADT.Show
import Data.Hashable
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Some
import Data.Text (Text)
-- Intentionally limit use of Typeable to avoid casting or typeOf usage
import Data.Typeable ((:~:)(..))
import TextShow (TextShow(..))
import qualified TextShow as TS
import Prelude

import Duckling.AmountOfMoney.Types (AmountOfMoneyData)
import Duckling.Distance.Types (DistanceData)
import Duckling.Duration.Types (DurationData)
import Duckling.Email.Types (EmailData)
import Duckling.Numeral.Types (NumeralData)
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
-- Dimension

-- | GADT for differentiating between dimensions
-- Each dimension should have its own constructor and provide the data structure
-- for its parsed data
data Dimension a where
  RegexMatch :: Dimension GroupMatch
  AmountOfMoney :: Dimension AmountOfMoneyData
  Distance :: Dimension DistanceData
  Duration :: Dimension DurationData
  Email :: Dimension EmailData
  Numeral :: Dimension NumeralData
  Ordinal :: Dimension OrdinalData
  PhoneNumber :: Dimension PhoneNumberData
  Quantity :: Dimension QuantityData
  Temperature :: Dimension TemperatureData
  Time :: Dimension TimeData
  TimeGrain :: Dimension Grain
  Url :: Dimension UrlData
  Volume :: Dimension VolumeData

-- Show
instance Show (Dimension a) where
  show RegexMatch = "RegexMatch"
  show Distance = "Distance"
  show Duration = "Duration"
  show Email = "Email"
  show AmountOfMoney = "AmountOfMoney"
  show Numeral = "Numeral"
  show Ordinal = "Ordinal"
  show PhoneNumber = "PhoneNumber"
  show Quantity = "Quantity"
  show Temperature = "Temperature"
  show Time = "Time"
  show TimeGrain = "TimeGrain"
  show Url = "Url"
  show Volume = "Volume"
instance GShow Dimension where gshowsPrec = showsPrec

-- TextShow
instance TextShow (Dimension a) where
  showb d = TS.fromString $ show d
instance TextShow (Some Dimension) where
  showb (This d) = showb d

-- Hashable
instance Hashable (Some Dimension) where
  hashWithSalt s (This a) = hashWithSalt s a
instance Hashable (Dimension a) where
  hashWithSalt s RegexMatch  = hashWithSalt s (0::Int)
  hashWithSalt s Distance    = hashWithSalt s (1::Int)
  hashWithSalt s Duration    = hashWithSalt s (2::Int)
  hashWithSalt s Email       = hashWithSalt s (3::Int)
  hashWithSalt s AmountOfMoney     = hashWithSalt s (4::Int)
  hashWithSalt s Numeral     = hashWithSalt s (5::Int)
  hashWithSalt s Ordinal     = hashWithSalt s (6::Int)
  hashWithSalt s PhoneNumber = hashWithSalt s (7::Int)
  hashWithSalt s Quantity    = hashWithSalt s (8::Int)
  hashWithSalt s Temperature = hashWithSalt s (9::Int)
  hashWithSalt s Time        = hashWithSalt s (10::Int)
  hashWithSalt s TimeGrain   = hashWithSalt s (11::Int)
  hashWithSalt s Url         = hashWithSalt s (12::Int)
  hashWithSalt s Volume      = hashWithSalt s (13::Int)


toName :: Dimension a -> Text
toName RegexMatch = "regex"
toName Distance = "distance"
toName Duration = "duration"
toName Email = "email"
toName AmountOfMoney = "amount-of-money"
toName Numeral = "number"
toName Ordinal = "ordinal"
toName PhoneNumber = "phone-number"
toName Quantity = "quantity"
toName Temperature = "temperature"
toName Time = "time"
toName TimeGrain = "time-grain"
toName Url = "url"
toName Volume = "volume"

fromName :: Text -> Maybe (Some Dimension)
fromName name = HashMap.lookup name m
  where
    m = HashMap.fromList
      [ ("amount-of-money", This AmountOfMoney)
      , ("distance", This Distance)
      , ("duration", This Duration)
      , ("email", This Email)
      , ("number", This Numeral)
      , ("ordinal", This Ordinal)
      , ("phone-number", This PhoneNumber)
      , ("quantity", This Quantity)
      , ("temperature", This Temperature)
      , ("time", This Time)
      , ("url", This Url)
      , ("volume", This Volume)
      ]

instance GEq Dimension where
  geq RegexMatch RegexMatch = Just Refl
  geq RegexMatch _ = Nothing
  geq Distance Distance = Just Refl
  geq Distance _ = Nothing
  geq Duration Duration = Just Refl
  geq Duration _ = Nothing
  geq Email Email = Just Refl
  geq Email _ = Nothing
  geq AmountOfMoney AmountOfMoney = Just Refl
  geq AmountOfMoney _ = Nothing
  geq Numeral Numeral = Just Refl
  geq Numeral _ = Nothing
  geq Ordinal Ordinal = Just Refl
  geq Ordinal _ = Nothing
  geq PhoneNumber PhoneNumber = Just Refl
  geq PhoneNumber _ = Nothing
  geq Quantity Quantity = Just Refl
  geq Quantity _ = Nothing
  geq Temperature Temperature = Just Refl
  geq Temperature _ = Nothing
  geq Time Time = Just Refl
  geq Time _ = Nothing
  geq TimeGrain TimeGrain = Just Refl
  geq TimeGrain _ = Nothing
  geq Url Url = Just Refl
  geq Url _ = Nothing
  geq Volume Volume = Just Refl
  geq Volume _ = Nothing
