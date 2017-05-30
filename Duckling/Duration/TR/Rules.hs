-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.TR.Rules
  ( rules
  ) where

import Control.Monad (join)
import Data.String
import qualified Data.Text as Text
import Prelude

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Numeral.Types (NumeralData (..))
import qualified Duckling.Numeral.Types as TNumeral
import Duckling.Regex.Types
import qualified Duckling.TimeGrain.Types as TG
import Duckling.Types

ruleDurationQuarterOfAnHour :: Rule
ruleDurationQuarterOfAnHour = Rule
  { name = "quarter of an hour"
  , pattern = [ regex "(1/4\\s?sa(at)?|\x00e7eyrek saat)" ]
  , prod = \_ -> Just . Token Duration $ duration TG.Minute 15
  }

ruleDurationHalfAnHour :: Rule
ruleDurationHalfAnHour = Rule
  { name = "half an hour"
  , pattern = [regex "(1/2\\s?sa(at)?|yar\305m saat)"]
  , prod = \_ -> Just . Token Duration $ duration TG.Minute 30
  }

ruleDurationThreeQuartersOfAnHour :: Rule
ruleDurationThreeQuartersOfAnHour = Rule
  { name = "three-quarters of an hour"
  , pattern = [regex "(3/4\\s?sa(at)?|\x00fc\x00e7e \231eyrek sa(at)?)"]
  , prod = \_ -> Just . Token Duration $ duration TG.Minute 45
  }

ruleDurationFortnight :: Rule
ruleDurationFortnight = Rule
  { name = "fortnight"
  , pattern = [regex "iki hafta"]
  , prod = \_ -> Just . Token Duration $ duration TG.Day 14
  }

ruleNumeralQuotes :: Rule
ruleNumeralQuotes = Rule
  { name = "<integer> + '\""
  , pattern =
    [ Predicate isNatural
    , regex "(['\"])"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v}):
       Token RegexMatch (GroupMatch (x:_)):
       _) -> case x of
         "'"  -> Just . Token Duration . duration TG.Minute $ floor v
         "\"" -> Just . Token Duration . duration TG.Second $ floor v
         _    -> Nothing
      _ -> Nothing
  }


ruleDurationDotNumeralHours :: Rule
ruleDurationDotNumeralHours = Rule
  { name = "number.number hours"
  , pattern = [regex "(\\d+)(\\.|,)(\\d+) *sa(at)?"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (h:d:_)):_) -> do
        hh <- parseInt h
        dec <- parseInt d
        let divisor = floor $ (fromIntegral (10 :: Integer) :: Float) **
                        fromIntegral (Text.length d - 1)
            numerator = fromIntegral $ 6 * dec
        Just . Token Duration . duration TG.Minute $
          60 * hh + quot numerator divisor
      _ -> Nothing
  }

ruleDurationAndHalfHour :: Rule
ruleDurationAndHalfHour = Rule
  { name = "<integer> and an half hour"
  , pattern =
    [ Predicate isNatural
    , regex "bu\x00e7euk sa(at)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v}):_) ->
        Just . Token Duration . duration TG.Minute $ 30 + 60 * floor v
      _ -> Nothing
  }


ruleDurationPrecision :: Rule
ruleDurationPrecision = Rule
  { name = "<duration> about|exactly"
  , pattern =
    [ regex "(gibi|civar\305nda|yakla\x015f\305k|tam (olarak)?)"
    , dimension Duration
    ]
    , prod = \tokens -> case tokens of
        (_:token:_) -> Just token
        _           -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleDurationQuarterOfAnHour
  , ruleDurationHalfAnHour
  , ruleDurationThreeQuartersOfAnHour
  , ruleDurationFortnight
  , ruleDurationDotNumeralHours
  , ruleDurationAndHalfHour
  , ruleDurationPrecision
  , ruleNumeralQuotes
  ]
