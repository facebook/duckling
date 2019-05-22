-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.IT.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
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

ruleNumeralsPrefixWithNegativeOrMinus :: Rule
ruleNumeralsPrefixWithNegativeOrMinus = Rule
  { name = "numbers prefix with -, negative or minus"
  , pattern =
    [ regex "-|meno|negativo"
    , Predicate isPositive
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral nd:_) -> double (TNumeral.value nd * (-1))
      _ -> Nothing
  }

ruleDecimalWithThousandsSeparator :: Rule
ruleDecimalWithThousandsSeparator = Rule
  { name = "decimal with thousands separator"
  , pattern =
    [ regex "(\\d+(([\\. ])\\d\\d\\d)+,\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_:sep:_)):
       _) -> let fmt = Text.replace "," "." $ Text.replace sep Text.empty match
        in parseDouble fmt >>= double
      _ -> Nothing
  }

ruleDecimalNumeral :: Rule
ruleDecimalNumeral = Rule
  { name = "decimal number"
  , pattern =
    [ regex "(\\d*,\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> parseDecimal False match
      _ -> Nothing
  }

tensMap :: HashMap Text Integer
tensMap = HashMap.fromList
  [ ("venti", 20)
  , ("trenta", 30)
  , ("quaranta", 40)
  , ("cinquanta", 50)
  , ("sessanta", 60)
  , ("settanta", 70)
  , ("ottanta", 80)
  , ("novanta", 90)
  ]

ruleNumeral2 :: Rule
ruleNumeral2 = Rule
  { name = "number (20..90)"
  , pattern =
    [ regex "(venti|trenta|quaranta|cinquanta|sessanta|settanta|ottanta|novanta)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) tensMap >>= integer
      _ -> Nothing
  }

zeroNineteenMap :: HashMap Text Integer
zeroNineteenMap = HashMap.fromList
  [ ("zero", 0)
  , ("niente", 0)
  , ("nulla", 0)
  , ("un", 1)
  , ("uno", 1)
  , ("due", 2)
  , ("tre", 3)
  , ("quattro", 4)
  , ("cinque", 5)
  , ("sei", 6)
  , ("sette", 7)
  , ("otto", 8)
  , ("nove", 9)
  , ("dieci", 10)
  , ("undici", 11)
  , ("dodici", 12)
  , ("tredici", 13)
  , ("quattordici", 14)
  , ("quindici", 15)
  , ("sedici", 16)
  , ("diciassette", 17)
  , ("diciotto", 18)
  , ("diciannove", 19)
  ]

ruleNumeral :: Rule
ruleNumeral = Rule
  { name = "number (0..19)"
  , pattern =
    [ regex "(zero|nulla|niente|uno|due|tredici|tre|quattro|cinque|sei|sette|otto|nove|dieci|undici|dodici|quattordici|quindici|sedici|diciassette|diciotto|diciannove|un)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) zeroNineteenMap >>= integer
      _ -> Nothing
  }

hundredsMap :: HashMap Text Integer
hundredsMap = HashMap.fromList
  [ ("cento", 100)
  , ("duecento", 200)
  , ("trecento", 300)
  , ("quattrocento", 400)
  , ("cinquecento", 500)
  , ("seicento", 600)
  , ("settecento", 700)
  , ("ottocento", 800)
  , ("novecento", 900)
  , ("mila", 1000)
  , ("mille", 1000)
  ]

ruleNumeral5 :: Rule
ruleNumeral5 = Rule
  { name = "number 100..1000 "
  , pattern =
    [ regex "(due|tre|quattro|cinque|sei|sette|otto|nove)?cento|mil(a|le)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) hundredsMap >>= integer
      _ -> Nothing
  }

ruleNumeral3 :: Rule
ruleNumeral3 = Rule
  { name = "number (21..29 31..39 41..49 51..59 61..69 71..79 81..89 91..99)"
  , pattern =
    [ oneOf [70, 20, 60, 50, 40, 90, 30, 80]
    , regex "e"
    , numberBetween 1 10
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v1}:
       _:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleNumeralsSuffixesKMG :: Rule
ruleNumeralsSuffixesKMG = Rule
  { name = "numbers suffixes (K, M, G)"
  , pattern =
    [ dimension Numeral
    , regex "([kmg])(?=[\\W\\$€]|$)"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
         "k" -> double $ v * 1e3
         "m" -> double $ v * 1e6
         "g" -> double $ v * 1e9
         _   -> Nothing
      _ -> Nothing
  }

twentyoneNinetynineMap :: HashMap Text Integer
twentyoneNinetynineMap = HashMap.fromList
  [ ("ventuno", 21)
  , ("ventidue", 22)
  , ("ventitre", 23)
  , ("ventitré", 23)
  , ("ventiquattro", 24)
  , ("venticinque", 25)
  , ("ventisei", 26)
  , ("ventisette", 27)
  , ("ventotto", 28)
  , ("ventinove", 29)
  , ("trentuno", 31)
  , ("trentadue", 32)
  , ("trentatre", 33)
  , ("trentatré", 33)
  , ("trentaquattro", 34)
  , ("trentacinque", 35)
  , ("trentasei", 36)
  , ("trentasette", 37)
  , ("trentotto", 38)
  , ("trentanove", 39)
  , ("quarantuno", 41)
  , ("quarantadue", 42)
  , ("quarantatre", 43)
  , ("quarantatré", 43)
  , ("quarantaquattro", 44)
  , ("quarantacinque", 45)
  , ("quarantasei", 46)
  , ("quarantasette", 47)
  , ("quarantotto", 48)
  , ("quarantanove", 49)
  , ("cinquantuno", 51)
  , ("cinquantadue", 52)
  , ("cinquantatre", 53)
  , ("cinquantatré", 53)
  , ("cinquantaquattro", 54)
  , ("cinquantacinque", 55)
  , ("cinquantasei", 56)
  , ("cinquantasette", 57)
  , ("cinquantotto", 58)
  , ("cinquantanove", 59)
  , ("sessantuno", 61)
  , ("sessantadue", 62)
  , ("sessantatré", 63)
  , ("sessantatre", 63)
  , ("sessantaquattro", 64)
  , ("sessantacinque", 65)
  , ("sessantasei", 66)
  , ("sessantasette", 67)
  , ("sessantotto", 68)
  , ("sessantanove", 69)
  , ("settantuno", 71)
  , ("settantadue", 72)
  , ("settantatré", 73)
  , ("settantatre", 73)
  , ("settantaquattro", 74)
  , ("settantacinque", 75)
  , ("settantasei", 76)
  , ("settantasette", 77)
  , ("settantotto", 78)
  , ("settantanove", 79)
  , ("ottantuno", 81)
  , ("ottantadue", 82)
  , ("ottantatré", 83)
  , ("ottantatre", 83)
  , ("ottantaquattro", 84)
  , ("ottantacinque", 85)
  , ("ottantasei", 86)
  , ("ottantasette", 87)
  , ("ottantotto", 88)
  , ("ottantanove", 89)
  , ("novantuno", 91)
  , ("novantadue", 92)
  , ("novantatre", 93)
  , ("novantatré", 93)
  , ("novantaquattro", 94)
  , ("novantacinque", 95)
  , ("novantasei", 96)
  , ("novantasette", 97)
  , ("novantotto", 98)
  , ("novantanove", 99)
  ]

ruleNumeral4 :: Rule
ruleNumeral4 = Rule
  { name = "number (21..29 31..39 41..49 51..59 61..69 71..79 81..89 91..99)"
  , pattern =
    [ regex "((venti|trenta|quaranta|cinquanta|sessanta|settanta|ottanta|novanta)(due|tre|tré|quattro|cinque|sei|sette|nove))|((vent|trent|quarant|cinquant|sessant|settant|ottant|novant)(uno|otto))"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) twentyoneNinetynineMap >>= integer
      _ -> Nothing
  }

ruleNumerals :: Rule
ruleNumerals = Rule
  { name = "numbers 200..999"
  , pattern =
    [ numberBetween 2 10
    , numberWith TNumeral.value (== 100)
    , numberBetween 0 100
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v1}:
       Token Numeral NumeralData{TNumeral.value = v2}:
       Token Numeral NumeralData{TNumeral.value = v3}:
       _) -> double $ v1 * v2 + v3
      _ -> Nothing
  }

ruleIntegerWithThousandsSeparator :: Rule
ruleIntegerWithThousandsSeparator = Rule
  { name = "integer with thousands separator ."
  , pattern =
    [ regex "(\\d{1,3}(([\\. ])\\d\\d\\d){1,5})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_:sep:_)):
       _) -> let fmt = Text.replace sep Text.empty match
        in parseDouble fmt >>= double
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleDecimalNumeral
  , ruleDecimalWithThousandsSeparator
  , ruleIntegerWithThousandsSeparator
  , ruleNumeral
  , ruleNumeral2
  , ruleNumeral3
  , ruleNumeral4
  , ruleNumeral5
  , ruleNumerals
  , ruleNumeralsPrefixWithNegativeOrMinus
  , ruleNumeralsSuffixesKMG
  ]
