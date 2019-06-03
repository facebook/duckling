-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Duckling.Quantity.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Hashable
import Data.Text (Text)
import GHC.Generics
import Prelude
import Duckling.Resolve (Resolve(..))
import qualified Data.HashMap.Strict as H
import qualified Data.Text as Text

data Unit
  = Bowl
  | Cup
  | Custom Text
  | Dish
  | Gram
  | Ounce
  | Pint
  | Pound
  | Quart
  | Tablespoon
  | Teaspoon
  | Unnamed
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance ToJSON Unit where
  toJSON (Custom x) = String $ Text.toLower x
  toJSON x = String . Text.toLower . Text.pack $ show x

data QuantityData = QuantityData
  { unit :: Maybe Unit
  , value :: Maybe Double
  , aproduct :: Maybe Text
  , minValue :: Maybe Double
  , maxValue :: Maybe Double
  } deriving (Eq, Generic, Hashable, Ord, Show, NFData)


instance Resolve QuantityData where
  type ResolvedValue QuantityData = QuantityValue

  resolve _ _ QuantityData {value = Just value
                         , unit = Just unit
                         , aproduct = aproduct}
   = Just (simple unit value aproduct, False)

  resolve _ _ QuantityData {value = Nothing
                         , unit = Just unit
                         , aproduct = aproduct
                         , minValue = Just from
                         , maxValue = Just to}
   = Just (between unit (from, to) aproduct, False)

  resolve _ _ QuantityData {value = Nothing
                         , unit = Just unit
                         , aproduct = aproduct
                         , minValue = Just from
                         , maxValue = Nothing}
   = Just (above unit from aproduct, False)

  resolve _ _ QuantityData {value = Nothing
                         , unit = Just unit
                         , aproduct = aproduct
                         , minValue = Nothing
                         , maxValue = Just to}
   = Just (under unit to aproduct, False)

  resolve _ _ _ = Nothing

data IntervalDirection = Above | Under
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

data SingleValue = SingleValue
    { vUnit :: Unit
    , vValue :: Double
    , vProduct :: Maybe Text
    }
    deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance ToJSON SingleValue where
    toJSON (SingleValue unit value aproduct) = object $
      [ "value" .= value
      , "unit" .= unit
      ]
      ++ [ "product" .= p | Just p <- [aproduct] ]


data QuantityValue
  = SimpleValue SingleValue
  | IntervalValue (SingleValue, SingleValue)
  | OpenIntervalValue (SingleValue, IntervalDirection)
  deriving (Eq, Ord, Show)

instance ToJSON QuantityValue where
  toJSON (SimpleValue value) = case toJSON value of
    Object o -> Object $ H.insert "type" (toJSON ("value" :: Text)) o
    _ -> Object H.empty
  toJSON (IntervalValue (from, to)) = object
    [ "type" .= ("interval" :: Text)
    , "from" .= toJSON from
    , "to" .= toJSON to
    ]
  toJSON (OpenIntervalValue (from, Above)) = object
    [ "type" .= ("interval" :: Text)
    , "from" .= toJSON from
    ]
  toJSON (OpenIntervalValue (to, Under)) = object
    [ "type" .= ("interval" :: Text)
    , "to" .= toJSON to
    ]
-- -----------------------------------------------------------------
-- Value helpers

simple :: Unit -> Double -> Maybe Text -> QuantityValue
simple u v p = SimpleValue $ single u v p

between :: Unit -> (Double, Double) -> Maybe Text -> QuantityValue
between u (from,to) p = IntervalValue (single u from p, single u to p)

above :: Unit -> Double -> Maybe Text -> QuantityValue
above = openInterval Above

under :: Unit -> Double -> Maybe Text -> QuantityValue
under = openInterval Under

openInterval :: IntervalDirection
                     -> Unit
                     -> Double
                     -> Maybe Text
                     -> QuantityValue
openInterval direction u v p = OpenIntervalValue (single u v p, direction)

single :: Unit -> Double -> Maybe Text -> SingleValue
single u v p = SingleValue {vUnit = u, vValue = v, vProduct = p}
