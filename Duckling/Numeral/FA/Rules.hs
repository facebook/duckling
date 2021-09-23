-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Numeral.FA.Rules
  ( rules ) where

import Control.Monad (join)
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

zeroNineteenMap :: HashMap Text Integer
zeroNineteenMap = HashMap.fromList
  [ ("صفر", 0)
  , ("یک", 1)
  , ("دو", 2)
  , ("سه", 3)
  , ("چهار", 4)
  , ("پنج", 5)
  , ("شش", 6)
  , ("شیش", 6)
  , ("هفت", 7)
  , ("هشت", 8)
  , ("نه", 9)
  , ("ده", 10)
  , ("یازده", 11)
  , ("دوازده", 12)
  , ("سیزده", 13)
  , ("چهارده", 14)
  , ("پانزده", 15)
  , ("پونزده", 15)
  , ("شانزده", 16)
  , ("شونزده", 16)
  , ("هفده", 17)
  , ("هیفده", 17)
  , ("هجده", 18)
  , ("هیجده", 18)
  , ("نوزده", 19)
  ]

ruleToNineteen :: Rule
ruleToNineteen = Rule
  { name = "integer (0..19)"
  , pattern =
    [ regex "(صفر|یک|سه|چهارده|چهار|پنج|شی?ش|هفت|هشت|نه|یازده|دوازده|سیزده|پ(ا|و)نزده|ش(ا|و)نزده|هی?فده|هی?جده|نوزده|ده|دو)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        let x = Text.toLower match in
        (HashMap.lookup x zeroNineteenMap >>= integer)
      _ -> Nothing
  }

tensMap :: HashMap Text Integer
tensMap = HashMap.fromList
  [ ( "بیست"  , 20 )
  , ( "سی"  , 30 )
  , ( "چهل"  , 40 )
  , ( "پنجاه"   , 50 )
  , ( "شصت"   , 60 )
  , ( "هفتاد" , 70 )
  , ( "هشتاد"  , 80 )
  , ( "نود"  , 90 )
  , ( "صد"  , 100 )
  , ( "دویست"  , 200 )
  , ( "سیصد"  , 300 )
  , ( "سی صد"  , 300 )
  , ( "چهارصد"  , 400 )
  , ( "چهار صد"  , 400 )
  , ( "پانصد"  , 500 )
  , ( "پونصد"  , 500 )
  , ( "شیشصد"  , 600 )
  , ( "شیش صد"  , 600 )
  , ( "ششصد"  , 600 )
  , ( "شش صد"  , 600 )
  , ( "هفتصد"  , 700 )
  , ( "هفت صد"  , 700 )
  , ( "هشتصد"  , 800 )
  , ( "هشت صد"  , 800 )
  , ( "نهصد"  , 900 )
  , ( "نه صد"  , 900 )
  ]

ruleTens :: Rule
ruleTens = Rule
  { name = "integer (20..90)"
  , pattern =
    [ regex "(دویست|(سی|چهار|پان|پون|شی?ش|هفت|هشت|نه)? ?صد|بیست|سی|چهل|پنجاه|شصت|هفتاد|هشتاد|نود)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) tensMap >>= integer
      _ -> Nothing
  }

rulePowersOfTen :: Rule
rulePowersOfTen = Rule
  { name = "powers of tens"
  , pattern =
    [ regex "(هزار|میلیون|ملیون|میلیارد)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "هزار"  -> double 1e3 >>= withGrain 2 >>= withMultipliable
        "میلیون" -> double 1e6 >>= withGrain 3 >>= withMultipliable
        "ملیون"  -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "میلیارد"  -> double 1e9 >>= withGrain 9 >>= withMultipliable
        _          -> Nothing
      _ -> Nothing
  }

ruleCompositeTens :: Rule
ruleCompositeTens = Rule
  { name = "integer 21..99"
  , pattern =
    [ oneOf [20,30..90]
    , regex "و"
    , numberBetween 1 10
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = tens}:
       _:
       Token Numeral NumeralData{TNumeral.value = units}:
       _) -> double $ tens + units
      _ -> Nothing
  }

ruleCompositeHundred :: Rule
ruleCompositeHundred = Rule
  { name = "integer 21..99"
  , pattern =
    [ oneOf [100,200..900]
    , regex "و"
    , numberBetween 1 100
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = tens}:
       _:
       Token Numeral NumeralData{TNumeral.value = units}:
       _) -> double $ tens + units
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
    , regex "و"
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

numeralToStringMap :: HashMap Char String
numeralToStringMap =
  HashMap.fromList
    [ ('۰', "0")
    , ('۱', "1")
    , ('۲', "2")
    , ('۳', "3")
    , ('۴', "4")
    , ('۵', "5")
    , ('۶', "6")
    , ('۷', "7")
    , ('۸', "8")
    , ('۹', "9")
    ]

parseIntAsText :: Text -> Text
parseIntAsText =
  Text.pack
    . join
    . mapMaybe (`HashMap.lookup` numeralToStringMap)
    . Text.unpack

parseIntegerFromText :: Text -> Maybe Integer
parseIntegerFromText = parseInteger . parseIntAsText

ruleIntegerNumeric :: Rule
ruleIntegerNumeric = Rule
  { name = "Persian integer numeric"
  , pattern =
    [ regex "([۰-۹]{1,18})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseIntegerFromText match >>= integer
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleIntegerNumeric
  , ruleToNineteen
  , ruleTens
  , rulePowersOfTen
  , ruleCompositeTens
  , ruleCompositeHundred
  , ruleSum
  , ruleSumAnd
  , ruleMultiply
  ]
