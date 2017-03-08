-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Number.PL.Rules
  ( rules ) where

import Data.Maybe
import qualified Data.Text as Text
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Number.Helpers
import Duckling.Number.Types (NumberData (..))
import qualified Duckling.Number.Types as TNumber
import Duckling.Regex.Types
import Duckling.Types

ruleSixteen :: Rule
ruleSixteen = Rule
  { name = "sixteen"
  , pattern =
    [ regex "szesna(s|\x015b)(tu|cie|toma)"
    ]
  , prod = \_ -> integer 16
  }

ruleFourteen :: Rule
ruleFourteen = Rule
  { name = "fourteen"
  , pattern =
    [ regex "czterna(s|\x015b)(tu|cie|toma)"
    ]
  , prod = \_ -> integer 14
  }

ruleTwo :: Rule
ruleTwo = Rule
  { name = "two"
  , pattern =
    [ regex "dw(a|(o|\x00f3)(ch|m)|oma|iema|ie)"
    ]
  , prod = \_ -> integer 2
  }

ruleSixty :: Rule
ruleSixty = Rule
  { name = "sixty"
  , pattern =
    [ regex "sze(\x015b\x0107)dziesi(\x0105)t|sze(\x015b\x0107)dziesi(\x0119)ci(u|oma)"
    ]
  , prod = \_ -> integer 60
  }

ruleIntersectWithAnd :: Rule
ruleIntersectWithAnd = Rule
  { name = "intersect (with and)"
  , pattern =
    [ numberWith (fromMaybe 0 . TNumber.grain) (>1)
    , regex "i|a"
    , numberWith TNumber.multipliable not
    ]
  , prod = \tokens -> case tokens of
      (Token DNumber (NumberData {TNumber.value = val1, TNumber.grain = Just g}):
       _:
       Token DNumber (NumberData {TNumber.value = val2}):
       _) | (10 ** fromIntegral g) > val2 -> double $ val1 + val2
      _ -> Nothing
  }

ruleNumbersPrefixWithNegativeOrMinus :: Rule
ruleNumbersPrefixWithNegativeOrMinus = Rule
  { name = "numbers prefix with -, negative or minus"
  , pattern =
    [ regex "-|minus\\s?|negative\\s?"
    , dimension DNumber
    ]
  , prod = \tokens -> case tokens of
      (_:Token DNumber nd:_) -> double (TNumber.value nd * (-1))
      _ -> Nothing
  }

ruleOne :: Rule
ruleOne = Rule
  { name = "one"
  , pattern =
    [ regex "jed(en|nego|nemu|nym|nej|n(a|\x0105))"
    ]
  , prod = \_ -> integer 1
  }

ruleIntegerNumeric :: Rule
ruleIntegerNumeric = Rule
  { name = "integer (numeric)"
  , pattern =
    [ regex "(\\d{1,18})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        v <- toInteger <$> parseInt match
        integer v
      _ -> Nothing
  }

ruleTen :: Rule
ruleTen = Rule
  { name = "ten"
  , pattern =
    [ regex "dzisi(e|\x0119)(\x0107|c)(iu|ioma)?"
    ]
  , prod = \_ -> integer 10
  }

ruleSpecialCompositionForMissingHundredsLikeInOneTwentyTwo :: Rule
ruleSpecialCompositionForMissingHundredsLikeInOneTwentyTwo = Rule
  { name = "special composition for missing hundreds like in one twenty two"
  , pattern =
    [ numberBetween 1 10
    , numberBetween 10 100
    ]
  , prod = \tokens -> case tokens of
      (Token DNumber (NumberData {TNumber.value = v1}):
       Token DNumber (NumberData {TNumber.value = v2}):
       _) -> double (v1 * 100 + v2)
      _ -> Nothing
  }

ruleDecimalWithThousandsSeparator :: Rule
ruleDecimalWithThousandsSeparator = Rule
  { name = "decimal with thousands separator"
  , pattern =
    [ regex "(\\d+(,\\d\\d\\d)+\\.\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDouble (Text.replace (Text.singleton ',') Text.empty match) >>= double
      _ -> Nothing
  }

ruleNine :: Rule
ruleNine = Rule
  { name = "nine"
  , pattern =
    [ regex "dziewi(e|\x0119)(\x0107|c)(iu|ioma)?"
    ]
  , prod = \_ -> integer 9
  }

ruleNumber8 :: Rule
ruleNumber8 = Rule
  { name = "number 800"
  , pattern =
    [ regex "(osiem(set| setek))"
    ]
  , prod = \_ -> integer 800 >>= withGrain 2
  }

ruleTwelve :: Rule
ruleTwelve = Rule
  { name = "twelve"
  , pattern =
    [ regex "dwunast(u|oma)|dwana(\x015b|s)cie"
    ]
  , prod = \_ -> integer 12
  }

ruleDecimalNumber :: Rule
ruleDecimalNumber = Rule
  { name = "decimal number"
  , pattern =
    [ regex "(\\d*\\.\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> parseDecimal True match
      _ -> Nothing
  }

ruleFifteen :: Rule
ruleFifteen = Rule
  { name = "fifteen"
  , pattern =
    [ regex "pi(\x0119)tna(s|\x015b)(ta|tu|cie|toma)"
    ]
  , prod = \_ -> integer 15
  }

ruleEleven :: Rule
ruleEleven = Rule
  { name = "eleven"
  , pattern =
    [ regex "jedena(stu|(s|\x015b)cie|stoma)"
    ]
  , prod = \_ -> integer 11
  }

ruleThirteen :: Rule
ruleThirteen = Rule
  { name = "thirteen"
  , pattern =
    [ regex "trzyna(\x015b|s)(tu|cie|toma)"
    ]
  , prod = \_ -> integer 13
  }

ruleThirty :: Rule
ruleThirty = Rule
  { name = "thirty"
  , pattern =
    [ regex "trzydzie(\x015b)ci|trzydziest(u|oma)"
    ]
  , prod = \_ -> integer 30
  }

ruleNumber2 :: Rule
ruleNumber2 = Rule
  { name = "number 200"
  , pattern =
    [ regex "dwie((\x015b)cie| setki)"
    ]
  , prod = \_ -> integer 200 >>= withGrain 2
  }

ruleSeventeen :: Rule
ruleSeventeen = Rule
  { name = "seventeen"
  , pattern =
    [ regex "siedemna(s|\x015b)(tu|cie|toma)"
    ]
  , prod = \_ -> integer 17
  }

ruleNumber :: Rule
ruleNumber = Rule
  { name = "number 100"
  , pattern =
    [ regex "(sto|setki)"
    ]
  , prod = \_ -> integer 100 >>= withGrain 2
  }

ruleNumber9 :: Rule
ruleNumber9 = Rule
  { name = "number 900"
  , pattern =
    [ regex "dziewi(\x0119\x0107)(set| setek)"
    ]
  , prod = \_ -> integer 900 >>= withGrain 2
  }

ruleSingle :: Rule
ruleSingle = Rule
  { name = "single"
  , pattern =
    [ regex "pojedynczy"
    ]
  , prod = \_ -> integer 1
  }

ruleTwenty :: Rule
ruleTwenty = Rule
  { name = "twenty"
  , pattern =
    [ regex "dwadzie(\x015b|s)cia|dwudziest(u|oma)"
    ]
  , prod = \_ -> integer 20
  }

ruleAFew :: Rule
ruleAFew = Rule
  { name = "a few"
  , pattern =
    [ regex "kilk(a|u)"
    ]
  , prod = \_ -> integer 3
  }

ruleEight :: Rule
ruleEight = Rule
  { name = "eight"
  , pattern =
    [ regex "o(s|\x015b)(iem|miu|mioma)"
    ]
  , prod = \_ -> integer 8
  }

ruleNumber5 :: Rule
ruleNumber5 = Rule
  { name = "number 500"
  , pattern =
    [ regex "pi(\x0119\x0107)(set| setek)"
    ]
  , prod = \_ -> integer 500 >>= withGrain 2
  }

ruleNumber3 :: Rule
ruleNumber3 = Rule
  { name = "number 300"
  , pattern =
    [ regex "(trzy(sta| setki))"
    ]
  , prod = \_ -> integer 300 >>= withGrain 2
  }

ruleZero :: Rule
ruleZero = Rule
  { name = "zero"
  , pattern =
    [ regex "(zero|nic)"
    ]
  , prod = \_ -> integer 0
  }

ruleThousand :: Rule
ruleThousand = Rule
  { name = "thousand"
  , pattern =
    [ regex "ty(s|\x015b)i(a|\x0105|\x0119)c(e|y)?"
    ]
  , prod = \_ -> integer 1000 >>= withGrain 3 >>= withMultipliable
  }

ruleMillion :: Rule
ruleMillion = Rule
  { name = "million"
  , pattern =
    [ regex "milion(y|(\x00f3)w)?"
    ]
  , prod = \_ -> integer 1000000 >>= withGrain 6 >>= withMultipliable
  }

ruleIntersect :: Rule
ruleIntersect = Rule
  { name = "intersect"
  , pattern =
    [ numberWith (fromMaybe 0 . TNumber.grain) (>1)
    , numberWith TNumber.multipliable not
    ]
  , prod = \tokens -> case tokens of
      (Token DNumber (NumberData {TNumber.value = val1, TNumber.grain = Just g}):
       Token DNumber (NumberData {TNumber.value = val2}):
       _) | (10 ** fromIntegral g) > val2 -> double $ val1 + val2
      _ -> Nothing
  }

ruleMultiply :: Rule
ruleMultiply = Rule
  { name = "compose by multiplication"
  , pattern =
    [ dimension DNumber
    , numberWith TNumber.multipliable id
    ]
  , prod = \tokens -> case tokens of
      (token1:token2:_) -> multiply token1 token2
      _ -> Nothing
  }

ruleThree :: Rule
ruleThree = Rule
  { name = "three"
  , pattern =
    [ regex "trz(y|ema|ech)"
    ]
  , prod = \_ -> integer 3
  }

ruleFour :: Rule
ruleFour = Rule
  { name = "four"
  , pattern =
    [ regex "czter(ej|y|ech|em|ema)"
    ]
  , prod = \_ -> integer 4
  }

ruleNumbersSuffixesKMG :: Rule
ruleNumbersSuffixesKMG = Rule
  { name = "numbers suffixes (K, M, G)"
  , pattern =
    [ dimension DNumber
    , regex "([kmg])(?=[\\W\\$\x20ac]|$)"
    ]
  , prod = \tokens -> case tokens of
      (Token DNumber (NumberData {TNumber.value = v}):
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
         "k" -> double $ v * 1e3
         "m" -> double $ v * 1e6
         "g" -> double $ v * 1e9
         _   -> Nothing
      _ -> Nothing
  }

ruleNumber7 :: Rule
ruleNumber7 = Rule
  { name = "number 700"
  , pattern =
    [ regex "(siedem(set| setek))"
    ]
  , prod = \_ -> integer 700 >>= withGrain 2
  }

ruleAPair :: Rule
ruleAPair = Rule
  { name = "a pair"
  , pattern =
    [ regex "para?"
    ]
  , prod = \_ -> integer 2
  }

ruleCouple :: Rule
ruleCouple = Rule
  { name = "couple"
  , pattern =
    [ regex "pare"
    ]
  , prod = \_ -> integer 2
  }

ruleSix :: Rule
ruleSix = Rule
  { name = "six"
  , pattern =
    [ regex "sze(s|\x015b)(c|\x0107)(iu|oma|u)?"
    ]
  , prod = \_ -> integer 6
  }

ruleNumber6 :: Rule
ruleNumber6 = Rule
  { name = "number 600"
  , pattern =
    [ regex "(sze\x015b\x0107(set| setek))"
    ]
  , prod = \_ -> integer 600 >>= withGrain 2
  }

ruleNumber4 :: Rule
ruleNumber4 = Rule
  { name = "number 400"
  , pattern =
    [ regex "(cztery(sta| setki))"
    ]
  , prod = \_ -> integer 400 >>= withGrain 2
  }

ruleFive :: Rule
ruleFive = Rule
  { name = "five"
  , pattern =
    [ regex "pi(e|\x0119)(c|\x0107)(iu|oma|u)?"
    ]
  , prod = \_ -> integer 5
  }

ruleFourty :: Rule
ruleFourty = Rule
  { name = "fou?rty"
  , pattern =
    [ regex "czterdzie(\x015b)ci|czterdziest(u|oma)"
    ]
  , prod = \_ -> integer 40
  }

ruleDozen :: Rule
ruleDozen = Rule
  { name = "dozen"
  , pattern =
    [ regex "tuzin"
    ]
  , prod = \_ -> integer 12 >>= withMultipliable
  }

ruleSeven :: Rule
ruleSeven = Rule
  { name = "seven"
  , pattern =
    [ regex "sied(miu|em|mioma)"
    ]
  , prod = \_ -> integer 7
  }

ruleNineteen :: Rule
ruleNineteen = Rule
  { name = "nineteen"
  , pattern =
    [ regex "dziewietna(s|\x015b)(tu|cie|toma)"
    ]
  , prod = \_ -> integer 19
  }

ruleInteger2 :: Rule
ruleInteger2 = Rule
  { name = "integer 21..99"
  , pattern =
    [ oneOf [20, 30 .. 90]
    , numberBetween 1 10
    ]
  , prod = \tokens -> case tokens of
      (Token DNumber (NumberData {TNumber.value = v1}):
       Token DNumber (NumberData {TNumber.value = v2}):
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleEighteen :: Rule
ruleEighteen = Rule
  { name = "eighteen"
  , pattern =
    [ regex "osiemna(s|\x015b)(tu|cie|toma)"
    ]
  , prod = \_ -> integer 18
  }

ruleNumberDotNumber :: Rule
ruleNumberDotNumber = Rule
  { name = "number dot number"
  , pattern =
    [ dimension DNumber
    , regex "dot|point"
    , numberWith TNumber.grain isNothing
    ]
  , prod = \tokens -> case tokens of
      (Token DNumber nd1:_:Token DNumber nd2:_) ->
        double $ TNumber.value nd1 + decimalsToDouble (TNumber.value nd2)
      _ -> Nothing
  }

ruleIntegerWithThousandsSeparator :: Rule
ruleIntegerWithThousandsSeparator = Rule
  { name = "integer with thousands separator ,"
  , pattern =
    [ regex "(\\d{1,3}(,\\d\\d\\d){1,5})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDouble (Text.replace (Text.singleton ',') Text.empty match) >>= double
      _ -> Nothing
  }

ruleFifty :: Rule
ruleFifty = Rule
  { name = "fifty"
  , pattern =
    [ regex "pi(\x0119\x0107)dziesi(\x0105)t|pi(\x0119\x0107)dziesi(\x0119)ci(u|oma)"
    ]
  , prod = \_ -> integer 50
  }

rules :: [Rule]
rules =
  [ ruleAFew
  , ruleAPair
  , ruleCouple
  , ruleDecimalNumber
  , ruleDecimalWithThousandsSeparator
  , ruleDozen
  , ruleEight
  , ruleEighteen
  , ruleEleven
  , ruleFifteen
  , ruleFifty
  , ruleFive
  , ruleFour
  , ruleFourteen
  , ruleFourty
  , ruleInteger2
  , ruleIntegerNumeric
  , ruleIntegerWithThousandsSeparator
  , ruleIntersect
  , ruleIntersectWithAnd
  , ruleMillion
  , ruleMultiply
  , ruleNine
  , ruleNineteen
  , ruleNumber
  , ruleNumber2
  , ruleNumber3
  , ruleNumber4
  , ruleNumber5
  , ruleNumber6
  , ruleNumber7
  , ruleNumber8
  , ruleNumber9
  , ruleNumberDotNumber
  , ruleNumbersPrefixWithNegativeOrMinus
  , ruleNumbersSuffixesKMG
  , ruleOne
  , ruleSeven
  , ruleSeventeen
  , ruleSingle
  , ruleSix
  , ruleSixteen
  , ruleSixty
  , ruleSpecialCompositionForMissingHundredsLikeInOneTwentyTwo
  , ruleTen
  , ruleThirteen
  , ruleThirty
  , ruleThousand
  , ruleThree
  , ruleTwelve
  , ruleTwenty
  , ruleTwo
  , ruleZero
  ]
