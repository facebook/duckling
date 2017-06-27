-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Numeral.EN.Rules
  ( rules ) where

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

ruleIntegers :: Rule
ruleIntegers = Rule
  { name = "integer (numeric)"
  , pattern = [regex "(\\d{1,18})"]
  , prod = \tokens ->
      case tokens of
        (Token RegexMatch (GroupMatch (match:_)):_) -> do
          v <- parseInt match
          integer $ toInteger v
        _ -> Nothing
  }

ruleDozen :: Rule
ruleDozen = Rule
  { name = "a dozen of"
  , pattern =
    [ regex "(a )?dozens?( of)?"
    ]
  , prod = \_ -> integer 12 >>= withMultipliable >>= notOkForAnyTime
  }

zeroNineteenMap :: HashMap Text Integer
zeroNineteenMap = HashMap.fromList
  [ ( "naught", 0 )
  , ( "nil"   , 0 )
  , ( "nought", 0 )
  , ( "none"  , 0 )
  , ( "zero"  , 0 )
  , ( "zilch" , 0 )
  , ( "one"   , 1 )
  , ( "two", 2 )
  , ( "three", 3 )
  , ( "four", 4 )
  , ( "five", 5 )
  , ( "six", 6 )
  , ( "seven", 7 )
  , ( "eight", 8 )
  , ( "nine", 9 )
  , ( "ten", 10 )
  , ( "eleven", 11 )
  , ( "twelve", 12 )
  , ( "thirteen", 13 )
  , ( "fourteen", 14 )
  , ( "fifteen", 15 )
  , ( "sixteen", 16 )
  , ( "seventeen", 17 )
  , ( "eighteen", 18 )
  , ( "nineteen", 19 )
  ]

informalMap :: HashMap Text Integer
informalMap = HashMap.fromList
  [ ( "single", 1 )
  , ( "a couple", 2 )
  , ( "a couple of", 2 )
  , ( "couple", 2 )
  , ( "couples", 2 )
  , ( "couple of", 2 )
  , ( "couples of", 2 )
  , ( "a pair", 2 )
  , ( "a pair of", 2 )
  , ( "pair", 2 )
  , ( "pairs", 2 )
  , ( "pair of", 2 )
  , ( "pairs of", 2 )
  , ( "a few", 3 )
  , ( "few", 3 )
  ]

ruleToNineteen :: Rule
ruleToNineteen = Rule
  { name = "integer (0..19)"
  -- e.g. fourteen must be before four, otherwise four will always shadow fourteen
  , pattern =
    [ regex "(none|zilch|naught|nought|nil|zero|one|single|two|(a )?(pair|couple)s?( of)?|three|(a )?few|fourteen|four|fifteen|five|sixteen|six|seventeen|seven|eighteen|eight|nineteen|nine|ten|eleven|twelve|thirteen)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        let x = Text.toLower match in
        (HashMap.lookup x zeroNineteenMap >>= integer) <|>
        (HashMap.lookup x informalMap >>= integer >>= notOkForAnyTime)
      _ -> Nothing
  }

ruleTens :: Rule
ruleTens = Rule
  { name = "integer (20..90)"
  , pattern = [regex "(twenty|thirty|fou?rty|fifty|sixty|seventy|eighty|ninety)"]
  , prod = \tokens ->
      case tokens of
        (Token RegexMatch (GroupMatch (match:_)):_) ->
          case Text.toLower match of
            "twenty"  -> integer 20
            "thirty"  -> integer 30
            "forty"   -> integer 40
            "fourty"  -> integer 40
            "fifty"   -> integer 50
            "sixty"   -> integer 60
            "seventy" -> integer 70
            "eighty"  -> integer 80
            "ninety"  -> integer 90
            _         -> Nothing
        _ -> Nothing
  }

rulePowersOfTen :: Rule
rulePowersOfTen = Rule
  { name = "powers of tens"
  , pattern =
    [ regex "(hundred|thousand|million|billion)s?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "hundred"  -> double 1e2 >>= withGrain 2 >>= withMultipliable
        "thousand" -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "million"  -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "billion"  -> double 1e9 >>= withGrain 9 >>= withMultipliable
        _          -> Nothing
      _ -> Nothing
  }

ruleCompositeTens :: Rule
ruleCompositeTens = Rule
  { name = "integer 21..99"
  , pattern = [oneOf [20,30..90], numberBetween 1 10]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData { TNumeral.value = tens }) :
       Token Numeral (NumeralData { TNumeral.value = units }) :
       _) -> double (tens + units)
      _ -> Nothing
  }

ruleSkipHundreds :: Rule
ruleSkipHundreds = Rule
  { name = "one twenty two"
  , pattern = [numberBetween 1 10, numberBetween 10 100]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData { TNumeral.value = hundreds }) :
       Token Numeral (NumeralData { TNumeral.value = rest }) :
       _) -> double (hundreds*100 + rest)
      _ -> Nothing
  }

ruleDotSpelledOut :: Rule
ruleDotSpelledOut = Rule
  { name = "one point 2"
  , pattern =
    [ dimension Numeral
    , regex "point|dot"
    , numberWith TNumeral.grain isNothing
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
    [ regex "point|dot"
    , numberWith TNumeral.grain isNothing
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral nd:_) -> double . decimalsToDouble $ TNumeral.value nd
      _ -> Nothing
  }

ruleDecimals :: Rule
ruleDecimals = Rule
  { name = "decimal number"
  , pattern = [regex "(\\d*\\.\\d+)"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> parseDecimal True match
      _ -> Nothing
  }

ruleFractions :: Rule
ruleFractions = Rule
  { name = "fractional number"
  , pattern = [regex "(\\d+)/(\\d+)"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (numerator:denominator:_)):_) -> do
        n <- parseDecimal False numerator
        d <- parseDecimal False denominator
        divide n d
      _ -> Nothing
  }

ruleCommas :: Rule
ruleCommas = Rule
  { name = "comma-separated numbers"
  , pattern = [regex "(\\d+(,\\d\\d\\d)+(\\.\\d+)?)"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDouble (Text.replace (Text.singleton ',') Text.empty match) >>= double
      _ -> Nothing
  }

ruleSuffixes :: Rule
ruleSuffixes = Rule
  { name = "suffixes (K,M,G))"
  , pattern =
    [ dimension Numeral
    , regex "(k|m|g)(?=[\\W$\x20ac\x00a2\x00a3]|$)"
    ]
  , prod = \tokens ->
      case tokens of
        (Token Numeral nd : Token RegexMatch (GroupMatch (match : _)):_) -> do
          x <- case Text.toLower match of
            "k" -> Just 1e3
            "m" -> Just 1e6
            "g" -> Just 1e9
            _ -> Nothing
          double $ TNumeral.value nd * x
        _ -> Nothing
  }

ruleNegative :: Rule
ruleNegative = Rule
  { name = "negative numbers"
  , pattern =
    [ regex "-|minus\\s?|negative\\s?"
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral nd:_) -> double (TNumeral.value nd * (-1))
      _ -> Nothing
  }

ruleSum :: Rule
ruleSum = Rule
  { name = "intersect 2 numbers"
  , pattern =
    [ numberWith (fromMaybe 0 . TNumeral.grain) (>1)
    , numberWith TNumeral.multipliable not
    ]
  , prod = \tokens ->
      case tokens of
        (Token Numeral (NumeralData {TNumeral.value = val1, TNumeral.grain = Just g}):
         Token Numeral (NumeralData {TNumeral.value = val2}):
         _) | (10 ** fromIntegral g) > val2 -> double $ val1 + val2
        _ -> Nothing
  }

ruleSumAnd :: Rule
ruleSumAnd = Rule
  { name = "intersect 2 numbers (with and)"
  , pattern =
    [ numberWith (fromMaybe 0 . TNumeral.grain) (>1)
    , regex "and"
    , numberWith TNumeral.multipliable not
    ]
  , prod = \tokens ->
      case tokens of
        (Token Numeral (NumeralData {TNumeral.value = val1, TNumeral.grain = Just g}):
         _:
         Token Numeral (NumeralData {TNumeral.value = val2}):
         _) | (10 ** fromIntegral g) > val2 -> double $ val1 + val2
        _ -> Nothing
  }

ruleMultiply :: Rule
ruleMultiply = Rule
  { name = "compose by multiplication"
  , pattern =
    [ dimension Numeral
    , numberWith TNumeral.multipliable id
    ]
  , prod = \tokens -> case tokens of
      (token1:token2:_) -> multiply token1 token2
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleIntegers
  , ruleToNineteen
  , ruleTens
  , rulePowersOfTen
  , ruleCompositeTens
  , ruleSkipHundreds
  , ruleDotSpelledOut
  , ruleLeadingDotSpelledOut
  , ruleDecimals
  , ruleFractions
  , ruleCommas
  , ruleSuffixes
  , ruleNegative
  , ruleSum
  , ruleSumAnd
  , ruleMultiply
  , ruleDozen
  ]
