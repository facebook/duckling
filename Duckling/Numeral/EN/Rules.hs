-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Numeral.EN.Rules
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
    [ regex "(a )?dozens?( of)?"
    ]
  , prod = \_ -> integer 12 >>= withMultipliable >>= notOkForAnyTime
  }

zeroNineteenMap :: HashMap Text Integer
zeroNineteenMap = HashMap.fromList
  [ ( "naught"   , 0 )
  , ( "nil"      , 0 )
  , ( "nought"   , 0 )
  , ( "none"     , 0 )
  , ( "zero"     , 0 )
  , ( "zilch"    , 0 )
  , ( "one"      , 1 )
  , ( "two"      , 2 )
  , ( "three"    , 3 )
  , ( "four"     , 4 )
  , ( "five"     , 5 )
  , ( "six"      , 6 )
  , ( "seven"    , 7 )
  , ( "eight"    , 8 )
  , ( "nine"     , 9 )
  , ( "ten"      , 10 )
  , ( "eleven"   , 11 )
  , ( "twelve"   , 12 )
  , ( "thirteen" , 13 )
  , ( "fourteen" , 14 )
  , ( "fifteen"  , 15 )
  , ( "sixteen"  , 16 )
  , ( "seventeen", 17 )
  , ( "eighteen" , 18 )
  , ( "nineteen" , 19 )
  ]

informalMap :: HashMap Text Integer
informalMap = HashMap.fromList
  [ ( "single"     , 1 )
  , ( "a couple"   , 2 )
  , ( "a couple of", 2 )
  , ( "couple"     , 2 )
  , ( "couples"    , 2 )
  , ( "couple of"  , 2 )
  , ( "couples of" , 2 )
  , ( "a pair"     , 2 )
  , ( "a pair of"  , 2 )
  , ( "pair"       , 2 )
  , ( "pairs"      , 2 )
  , ( "pair of"    , 2 )
  , ( "pairs of"   , 2 )
  , ( "a few"      , 3 )
  , ( "few"        , 3 )
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

tensMap :: HashMap Text Integer
tensMap = HashMap.fromList
  [ ( "twenty"  , 20 )
  , ( "thirty"  , 30 )
  , ( "forty"   , 40 )
  , ( "fourty"  , 40 )
  , ( "fifty"   , 50 )
  , ( "sixty"   , 60 )
  , ( "seventy" , 70 )
  , ( "eighty"  , 80 )
  , ( "ninety"  , 90 )
  ]

ruleTens :: Rule
ruleTens = Rule
  { name = "integer (20..90)"
  , pattern =
    [ regex "(twenty|thirty|fou?rty|fifty|sixty|seventy|eighty|ninety)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) tensMap >>= integer
      _ -> Nothing
  }

rulePowersOfTen :: Rule
rulePowersOfTen = Rule
  { name = "powers of tens"
  , pattern = [regex "(hundred|thousand|lakh|million|crore|billion)s?"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match : _)) : _) ->
        case Text.toLower match of
          "hundred" -> double 1e2 >>= withGrain 2 >>= withMultipliable
          "thousand" -> double 1e3 >>= withGrain 3 >>= withMultipliable
          "lakh" -> double 1e5 >>= withGrain 5 >>= withMultipliable
          "million" -> double 1e6 >>= withGrain 6 >>= withMultipliable
          "crore" -> double 1e7 >>= withGrain 7 >>= withMultipliable
          "billion" -> double 1e9 >>= withGrain 9 >>= withMultipliable
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

ruleSkipHundreds1 :: Rule
ruleSkipHundreds1 = Rule
  { name = "one eleven"
  , pattern =
    [ regex "(one|two|three|four|five|six|seven|eight|nine)"
    , regex "(ten|eleven|twelve|thirteen|fourteen|fifteen|sixteen|seventeen|eighteen|nineteen|twenty|thirty|fou?rty|fifty|sixty|seventy|eighty|ninety)"
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
    [ regex "(one|two|three|four|five|six|seven|eight|nine)"
    , regex "(twenty|thirty|fou?rty|fifty|sixty|seventy|eighty|ninety)"
    , regex "(one|two|three|four|five|six|seven|eight|nine)"
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
    , regex "point|dot"
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
    [ regex "point|dot"
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
  { name = "suffixes (K,M,G))"
  , pattern =
    [ dimension Numeral
    , regex "(k|m|g)(?=[\\W$€¢£]|$)"
    ]
  , prod = \tokens -> case tokens of
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
    [ regex "(-|minus|negative)(?!\\s*-)"
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
    , regex "and"
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
  , ruleSkipHundreds1
  , ruleSkipHundreds2
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
