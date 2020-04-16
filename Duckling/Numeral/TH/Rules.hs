-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

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
  [ ( "ไม่มี", 0 )
  , ( "ศูนย์", 0 )
  , ( "หนึ่ง", 1 )
  , ( "เอ็ด", 1 )
  , ( "สอง", 2 )
  , ( "สาม", 3 )
  , ( "สี่", 4 )
  , ( "ห้า", 5 )
  , ( "หก", 6 )
  , ( "เจ็ด", 7 )
  , ( "แปด", 8 )
  , ( "เก้า", 9 )
  , ( "สิบ", 10 )
  , ( "สิบเอ็ด", 11 )
  , ( "สิบหนึ่ง", 11 )
  , ( "สิบสอง", 12 )
  , ( "สิบสาม", 13 )
  , ( "สิบสี่", 14 )
  , ( "สิบห้า", 15 )
  , ( "สิบหก", 16 )
  , ( "สิบเจ็ด", 17 )
  , ( "สิบแปด", 18 )
  , ( "สิบเก้า", 19 )
  ]

informalMap :: HashMap Text Integer
informalMap = HashMap.fromList
  [ ( "อันนึง", 1 )
  , ( "คู่นึง", 2 )
  , ( "คู่ของ", 2 )
  ]

ruleToNineteen :: Rule
ruleToNineteen = Rule
  { name = "integer (0..19)"
  -- e.g. fourteen must be before four, otherwise four will always shadow fourteen
  , pattern =
    [ regex "(ไม่มี|ศูนย์|สิบหนึ่ง|หนึ่ง|(คู่)s?( ของ)?|(คู่)s?( นึง)?|สิบเอ็ด|เอ็ด|สิบสอง|สิบสาม|สิบสี่|สิบห้า|สิบหก|สิบเจ็ด|สิบแปด|สิบเก้า|สอง|สาม|สี่|ห้า|หก|เจ็ด|แปด|เก้า|สิบ)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        let x = Text.toLower match in
        (HashMap.lookup x zeroNineteenMap >>= integer) <|>
        (HashMap.lookup x informalMap >>= integer >>= notOkForAnyTime)
      _ -> Nothing
  }

tensMap :: HashMap Text Integer
tensMap = HashMap.fromList
  [ ( "ยี่สิบ", 20 )
  , ( "สามสิบ", 30 )
  , ( "สี่สิบ", 40 )
  , ( "ห้าสิบ", 50 )
  , ( "หกสิบ", 60 )
  , ( "เจ็ดสิบ", 70 )
  , ( "แปดสิบ", 80 )
  , ( "เก้าสิบ", 90 )
  ]

ruleTens :: Rule
ruleTens = singleStringLookupRule tensMap "integer (20..90)" integer

digitsHundredTwentyToTwentyNineMap :: HashMap Text Integer
digitsHundredTwentyToTwentyNineMap = HashMap.fromList
  [ ( "ร้อยยี่สิบ", 120 )
  , ( "ร้อยยี่สิบเอ็ด", 121 )
  , ( "ร้อยยี่สิบหนึ่ง", 121 )
  , ( "ร้อยยี่สิบสอง", 122 )
  , ( "ร้อยยี่สิบสาม", 123 )
  , ( "ร้อยยี่สิบสี่", 124 )
  , ( "ร้อยยี่สิบห้า", 125 )
  , ( "ร้อยยี่สิบหก", 126 )
  , ( "ร้อยยี่สิบเจ็ด", 127 )
  , ( "ร้อยยี่สิบแปด", 128 )
  , ( "ร้อยยี่สิบเก้า", 129 )
  , ( "หนึ่งร้อยยี่สิบ", 120 )
  , ( "หนึ่งร้อยยี่สิบเอ็ด", 121 )
  , ( "หนึ่งร้อยยี่สิบหนึ่ง", 121 )
  , ( "หนึ่งร้อยยี่สิบสอง", 122 )
  , ( "หนึ่งร้อยยี่สิบสาม", 123 )
  , ( "หนึ่งร้อยยี่สิบสี่", 124 )
  , ( "หนึ่งร้อยยี่สิบห้า", 125 )
  , ( "หนึ่งร้อยยี่สิบหก", 126 )
  , ( "หนึ่งร้อยยี่สิบเจ็ด", 127 )
  , ( "หนึ่งร้อยยี่สิบแปด", 128 )
  , ( "หนึ่งร้อยยี่สิบเก้า", 129 )
  , ( "สองร้อยยี่สิบ", 220 )
  , ( "สองร้อยยี่สิบเอ็ด", 221 )
  , ( "สองร้อยยี่สิบหนึ่ง", 221 )
  , ( "สองร้อยยี่สิบสอง", 222 )
  , ( "สองร้อยยี่สิบสาม", 223 )
  , ( "สองร้อยยี่สิบสี่", 224 )
  , ( "สองร้อยยี่สิบห้า", 225 )
  , ( "สองร้อยยี่สิบหก", 226 )
  , ( "สองร้อยยี่สิบเจ็ด", 227 )
  , ( "สองร้อยยี่สิบแปด", 228 )
  , ( "สองร้อยยี่สิบเก้า", 229 )
  , ( "สามร้อยยี่สิบ", 320 )
  , ( "สามร้อยยี่สิบเอ็ด", 321 )
  , ( "สามร้อยยี่สิบหนึ่ง", 321 )
  , ( "สามร้อยยี่สิบสอง", 322 )
  , ( "สามร้อยยี่สิบสาม", 323 )
  , ( "สามร้อยยี่สิบสี่", 324 )
  , ( "สามร้อยยี่สิบห้า", 325 )
  , ( "สามร้อยยี่สิบหก", 326 )
  , ( "สามร้อยยี่สิบเจ็ด", 327 )
  , ( "สามร้อยยี่สิบแปด", 328 )
  , ( "สามร้อยยี่สิบเก้า", 329 )
  , ( "สี่ร้อยยี่สิบ", 420 )
  , ( "สี่ร้อยยี่สิบเอ็ด", 421 )
  , ( "สี่ร้อยยี่สิบหนึ่ง", 421 )
  , ( "สี่ร้อยยี่สิบสอง", 422 )
  , ( "สี่ร้อยยี่สิบสาม", 423 )
  , ( "สี่ร้อยยี่สิบสี่", 424 )
  , ( "สี่ร้อยยี่สิบห้า", 425 )
  , ( "สี่ร้อยยี่สิบหก", 426 )
  , ( "สี่ร้อยยี่สิบเจ็ด", 427 )
  , ( "สี่ร้อยยี่สิบแปด", 428 )
  , ( "สี่ร้อยยี่สิบเก้า", 429 )
  , ( "ห้าร้อยยี่สิบ", 520 )
  , ( "ห้าร้อยยี่สิบเอ็ด", 521 )
  , ( "ห้าร้อยยี่สิบหนึ่ง", 521 )
  , ( "ห้าร้อยยี่สิบสอง", 522 )
  , ( "ห้าร้อยยี่สิบสาม", 523 )
  , ( "ห้าร้อยยี่สิบสี่", 524 )
  , ( "ห้าร้อยยี่สิบห้า", 525 )
  , ( "ห้าร้อยยี่สิบหก", 526 )
  , ( "ห้าร้อยยี่สิบเจ็ด", 527 )
  , ( "ห้าร้อยยี่สิบแปด", 528 )
  , ( "ห้าร้อยยี่สิบเก้า", 529 )
  , ( "หกร้อยยี่สิบ", 620 )
  , ( "หกร้อยยี่สิบเอ็ด", 621 )
  , ( "หกร้อยยี่สิบหนึ่ง", 621 )
  , ( "หกร้อยยี่สิบสอง", 622 )
  , ( "หกร้อยยี่สิบสาม", 623 )
  , ( "หกร้อยยี่สิบสี่", 624 )
  , ( "หกร้อยยี่สิบห้า", 625 )
  , ( "หกร้อยยี่สิบหก", 626 )
  , ( "หกร้อยยี่สิบเจ็ด", 627 )
  , ( "หกร้อยยี่สิบแปด", 628 )
  , ( "หกร้อยยี่สิบเก้า", 629 )
  , ( "เจ็ดร้อยยี่สิบ", 720 )
  , ( "เจ็ดร้อยยี่สิบเอ็ด", 721 )
  , ( "เจ็ดร้อยยี่สิบหนึ่ง", 721 )
  , ( "เจ็ดร้อยยี่สิบสอง", 722 )
  , ( "เจ็ดร้อยยี่สิบสาม", 723 )
  , ( "เจ็ดร้อยยี่สิบสี่", 724 )
  , ( "เจ็ดร้อยยี่สิบห้า", 725 )
  , ( "เจ็ดร้อยยี่สิบหก", 726 )
  , ( "เจ็ดร้อยยี่สิบเจ็ด", 727 )
  , ( "เจ็ดร้อยยี่สิบแปด", 728 )
  , ( "เจ็ดร้อยยี่สิบเก้า", 729 )
  , ( "แปดร้อยยี่สิบ", 820 )
  , ( "แปดร้อยยี่สิบเอ็ด", 821 )
  , ( "แปดร้อยยี่สิบหนึ่ง", 821 )
  , ( "แปดร้อยยี่สิบสอง", 822 )
  , ( "แปดร้อยยี่สิบสาม", 823 )
  , ( "แปดร้อยยี่สิบสี่", 824 )
  , ( "แปดร้อยยี่สิบห้า", 825 )
  , ( "แปดร้อยยี่สิบหก", 826 )
  , ( "แปดร้อยยี่สิบเจ็ด", 827 )
  , ( "แปดร้อยยี่สิบแปด", 828 )
  , ( "แปดร้อยยี่สิบเก้า", 829 )
  , ( "เก้าร้อยยี่สิบ", 920 )
  , ( "เก้าร้อยยี่สิบเอ็ด", 921 )
  , ( "เก้าร้อยยี่สิบหนึ่ง", 921 )
  , ( "เก้าร้อยยี่สิบสอง", 922 )
  , ( "เก้าร้อยยี่สิบสาม", 923 )
  , ( "เก้าร้อยยี่สิบสี่", 924 )
  , ( "เก้าร้อยยี่สิบห้า", 925 )
  , ( "เก้าร้อยยี่สิบหก", 926 )
  , ( "เก้าร้อยยี่สิบเจ็ด", 927 )
  , ( "เก้าร้อยยี่สิบแปด", 928 )
  , ( "เก้าร้อยยี่สิบเก้า", 929 )
  ]

ruleXHundredTwentyToXHundredTwentyNine :: Rule
ruleXHundredTwentyToXHundredTwentyNine = singleStringLookupRule
  digitsHundredTwentyToTwentyNineMap "integer (x20,x21,...,x29)" integer

rulePowersOfTen :: Rule
rulePowersOfTen = Rule
  { name = "powers of tens"
  , pattern = [regex "(ร้อย|พัน|หมื่น|แสน|ล้าน|สิบล้าน|ร้อยล้าน|พันล้าน)"]
  , prod = \case
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
  , prod = \case
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
  , prod = \case
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
  , prod = \case
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
  , prod = \case
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
  , prod = \case
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
  , prod = \case
      (_:Token Numeral nd:_) -> double $ decimalsToDouble $ TNumeral.value nd
      _ -> Nothing
  }

ruleDecimals :: Rule
ruleDecimals = Rule
  { name = "decimal number"
  , pattern =
    [ regex "(\\d*\\.\\d+)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) -> parseDecimal True match
      _ -> Nothing
  }

ruleCommas :: Rule
ruleCommas = Rule
  { name = "comma-separated numbers"
  , pattern =
    [ regex "(\\d+(,\\d\\d\\d)+(\\.\\d+)?)"
    ]
  , prod = \case
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
  , prod = \case
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
  , prod = \case
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
  , prod = \case
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
  , prod = \case
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
  , prod = \case
      (token1:token2:_) -> multiply token1 token2
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleXHundredTwentyToXHundredTwentyNine
  , ruleSkipHundreds1
  , ruleSkipHundreds2
  , ruleToNineteen
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
