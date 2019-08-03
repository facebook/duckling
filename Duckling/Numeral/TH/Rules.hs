-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Numeral.TH.Rules
  ( rules
  ) where

import Control.Applicative ((<|>))
import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral

ruleDozen :: Rule
ruleDozen = Rule
  { name = "a dozen of"
  , pattern =
    [ regex "โหล?( ของ)?"
    ]
  , prod = \_ -> integer 12 >>= withMultipliable >>= notOkForAnyTime
  }

zeroNineteenMap :: HashMap Text Integer
zeroNineteenMap = HashMap.fromList
  [ ( "ไม่มี"     , 0 )
  , ( "ศูนย์"     , 0 )
  , ( "หนึ่ง"      , 1 )
  , ( "เอ็ด"      , 1 )
  , ( "สอง"      , 2 )
  , ( "สาม"    , 3 )
  , ( "สี่"     , 4 )
  , ( "ห้า"     , 5 )
  , ( "หก"      , 6 )
  , ( "เจ็ด"    , 7 )
  , ( "แปด"    , 8 )
  , ( "เก้า"     , 9 )
  , ( "สิบ"      , 10 )
  , ( "สิบเอ็ด"   , 11 )
  , ( "สิบสอง"   , 12 )
  , ( "สิบสาม" , 13 )
  , ( "สิบสี่" , 14 )
  , ( "สิบห้า"  , 15 )
  , ( "สิบหก"  , 16 )
  , ( "สิบเจ็ด", 17 )
  , ( "สิบแปด" , 18 )
  , ( "สิบเก้า" , 19 )
  ]

informalMap :: HashMap Text Integer
informalMap = HashMap.fromList
  [ ( "อันนึง"     , 1 )
  , ( "คู่นึง"   , 2 )
  , ( "คู่ของ", 2 )
  ]

ruleToNineteen :: Rule
ruleToNineteen = Rule
  { name = "integer (0..19)"
  -- e.g. fourteen must be before four, otherwise four will always shadow fourteen
  , pattern =
    [ regex "(ไม่มี|ศูนย์|หนึ่ง|(คู่)s?( ของ)?|(คู่)s?( นึง)?|สิบเอ็ด|สิบสอง|สิบสาม|สิบสี่|สิบห้า|สิบหก|สิบเจ็ด|สิบแปด|สิบเก้า|สอง|สาม|สี่|ห้า|หก|เจ็ด|แปด|เก้า|สิบ)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        let x = Text.toLower match in
        (HashMap.lookup x zeroNineteenMap >>= integer) <|>
        (HashMap.lookup x informalMap >>= integer >>= notOkForAnyTime)
      _ -> Nothing
  }

tensMap :: HashMap Text Integer
tensMap = HashMap.fromList
  [ ( "ยี่สิบ"  , 20 )
  , ( "สามสิบ"  , 30 )
  , ( "สี่สิบ"   , 40 )
  , ( "ห้าสิบ"   , 50 )
  , ( "หกสิบ"   , 60 )
  , ( "เจ็ดสิบ" , 70 )
  , ( "แปดสิบ"  , 80 )
  , ( "เก้าสิบ"  , 90 )
  ]

ruleTens :: Rule
ruleTens = Rule
  { name = "integer (20..90)"
  , pattern =
    [ regex "(ยี่สิบ|สามสิบ|สี่สิบ|ห้าสิบ|หกสิบ|เจ็ดสิบ|แปดสิบ|เก้าสิบ)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) tensMap >>= integer
      _ -> Nothing
  }

rulePowersOfTen :: Rule
rulePowersOfTen = Rule
  { name = "powers of tens"
  , pattern = [regex "(ร้อย|พัน|หมื่น|แสน|ล้าน|สิบล้าน|ร้อยล้าน|พันล้าน)s?"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match : _)) : _) ->
        case Text.toLower match of
          "ร้อย" -> double 1e2 >>= withGrain 2 >>= withMultipliable
          "พัน" -> double 1e3 >>= withGrain 3 >>= withMultipliable
          "หมื่น" -> double 1e4 >>= withGrain 4 >>= withMultipliable
          "แสน" -> double 1e5 >>= withGrain 5 >>= withMultipliable
          "ล้าน" -> double 1e6 >>= withGrain 6 >>= withMultipliable
          "สิบล้าน" -> double 1e7 >>= withGrain 7 >>= withMultipliable
          "ร้อยล้าน" -> double 1e8 >>= withGrain 8 >>= withMultipliable
          "พันล้าน" -> double 1e9 >>= withGrain 9 >>= withMultipliable
          _ -> Nothing
      _ -> Nothing
  }

ruleCompositeTens :: Rule
ruleCompositeTens = Rule
  { name = "integer 21..99"
  , pattern =
    [ oneOf [20,30..90]
    , regex "[\\s\\-]+"
    , numberBetween 1 10
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = tens}:
       _:
       Token Numeral NumeralData{TNumeral.value = units}:
       _) -> double $ tens + units
      _ -> Nothing
  }

ruleSumTenDigits :: Rule
ruleSumTenDigits = Rule
  { name = "สามสิบสี่"
  , pattern =
    [ regex "(ยี่สิบ|สามสิบ|สี่สิบ|ห้าสิบ|หกสิบ|เจ็ดสิบ|แปดสิบ|เก้าสิบ)"
    , regex "(หนึ่ง|เอ็ด|สอง|สาม|สี่|ห้า|หก|เจ็ด|แปด|เก้า|สิบ)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:_)):
       Token RegexMatch (GroupMatch (m2:_)):
       _) -> do
       let x1 = Text.toLower m1
       let x2 = Text.toLower m2
       hundreds <- HashMap.lookup x1 tensMap
       rest <- HashMap.lookup x2 zeroNineteenMap
       integer (hundreds + rest)
      _ -> Nothing
  }

ruleSkipHundreds1 :: Rule
ruleSkipHundreds1 = Rule
  { name = "one eleven"
  , pattern =
    [ regex "(หนึ่ง|สอง|สาม|สี่|ห้า|หก|เจ็ด|แปด|เก้า)"
    , regex "(สิบ|สิบเอ็ด|สิบสอง|สิบสาม|สิบสี่|สิบห้า|สิบหก|สิบเจ็ด|สิบแปด|สิบเก้า|ยี่สิบ|สามสิบ|สี่สิบ|ห้าสิบ|หกสิบ|เจ็ดสิบ|แปดสิบ|เก้าสิบ)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:_)):
       Token RegexMatch (GroupMatch (m2:_)):
       _) -> do
       let x1 = Text.toLower m1
       let x2 = Text.toLower m2
       hundreds <- HashMap.lookup x1 zeroNineteenMap
       rest <- HashMap.lookup x2 zeroNineteenMap <|> HashMap.lookup x2 tensMap
       integer (hundreds * 100 + rest)
      _ -> Nothing
  }

ruleSkipHundreds2 :: Rule
ruleSkipHundreds2 = Rule
  { name = "one twenty two"
  , pattern =
    [ regex "(หนึ่ง|สอง|สาม|สี่|ห้า|หก|เจ็ด|แปด|เก้า)"
    , regex "(ยี่สิบ|สามสิบ|สี่สิบ|ห้าสิบ|หกสิบ|เจ็ดสิบ|แปดสิบ|เก้าสิบ)"
    , regex "(หนึ่ง|สอง|สาม|สี่|ห้า|หก|เจ็ด|แปด|เก้า)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:_)):
       Token RegexMatch (GroupMatch (m2:_)):
       Token RegexMatch (GroupMatch (m3:_)):
       _) -> do
       let x1 = Text.toLower m1
       let x2 = Text.toLower m2
       let x3 = Text.toLower m3
       hundreds <- HashMap.lookup x1 zeroNineteenMap
       tens <- HashMap.lookup x2 tensMap
       rest <- HashMap.lookup x3 zeroNineteenMap
       integer (hundreds * 100 + tens + rest)
      _ -> Nothing
  }

ruleDotSpelledOut :: Rule
ruleDotSpelledOut = Rule
  { name = "one point 2"
  , pattern =
    [ dimension Numeral
    , regex "จุด"
    , Predicate $ not . hasGrain
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral nd1:_:Token Numeral nd2:_) ->
        double $ TNumeral.value nd1 + decimalsToDouble (TNumeral.value nd2)
      _ -> Nothing
  }

ruleLeadingDotSpelledOut :: Rule
ruleLeadingDotSpelledOut = Rule
  { name = "point 77"
  , pattern =
    [ regex "จุด"
    , Predicate $ not . hasGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral nd:_) -> double . decimalsToDouble $ TNumeral.value nd
      _ -> Nothing
  }

ruleDecimals :: Rule
ruleDecimals = Rule
  { name = "decimal number"
  , pattern =
    [ regex "(\\d*\\.\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> parseDecimal True match
      _ -> Nothing
  }

ruleCommas :: Rule
ruleCommas = Rule
  { name = "comma-separated numbers"
  , pattern =
    [ regex "(\\d+(,\\d\\d\\d)+(\\.\\d+)?)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDouble (Text.replace "," Text.empty match) >>= double
      _ -> Nothing
  }

ruleSuffixes :: Rule
ruleSuffixes = Rule
  { name = "suffixes (กิโลกรัม,กรัม))"
  , pattern =
    [ dimension Numeral
    , regex "(กิโลกรัม|กรัม)(?=[\\W$€¢£]|$)"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral nd : Token RegexMatch (GroupMatch (match : _)):_) -> do
        x <- case Text.toLower match of
          "กิโลกรัม" -> Just 1e3
          "กรัม" -> Just 1e1
          _ -> Nothing
        double $ TNumeral.value nd * x
      _ -> Nothing
  }

ruleNegative :: Rule
ruleNegative = Rule
  { name = "negative numbers"
  , pattern =
    [ regex "(-|ลบ)(?!\\s*-)"
    , Predicate isPositive
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral nd:_) -> double (TNumeral.value nd * (-1))
      _ -> Nothing
  }

ruleSum :: Rule
ruleSum = Rule
  { name = "intersect 2 numbers"
  , pattern =
    [ Predicate $ and . sequence [hasGrain, isPositive]
    , Predicate $ and . sequence [not . isMultipliable, isPositive]
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = val1, TNumeral.grain = Just g}:
       Token Numeral NumeralData{TNumeral.value = val2}:
       _) | (10 ** fromIntegral g) > val2 -> double $ val1 + val2
      _ -> Nothing
  }

ruleSumAnd :: Rule
ruleSumAnd = Rule
  { name = "intersect 2 numbers (with and)"
  , pattern =
    [ Predicate $ and . sequence [hasGrain, isPositive]
    , regex "และ"
    , Predicate $ and . sequence [not . isMultipliable, isPositive]
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = val1, TNumeral.grain = Just g}:
       _:
       Token Numeral NumeralData{TNumeral.value = val2}:
       _) | (10 ** fromIntegral g) > val2 -> double $ val1 + val2
      _ -> Nothing
  }

ruleMultiply :: Rule
ruleMultiply = Rule
  { name = "compose by multiplication"
  , pattern =
    [ dimension Numeral
    , Predicate isMultipliable
    ]
  , prod = \tokens -> case tokens of
      (token1:token2:_) -> multiply token1 token2
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleToNineteen
  , ruleTens
  , rulePowersOfTen
  , ruleCompositeTens
  , ruleSumTenDigits
  , ruleDotSpelledOut
  , ruleLeadingDotSpelledOut
  , ruleDecimals
  , ruleCommas
  , ruleSuffixes
  , ruleNegative
  , ruleSum
  , ruleSumAnd
  , ruleMultiply
  , ruleDozen
  ]
