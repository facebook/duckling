-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.TR.Rules
  ( rules ) where

import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Maybe
import qualified Data.Text as Text
import Data.Text (Text)
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import qualified Duckling.Numeral.Types as TNumeral
import Duckling.Regex.Types
import Duckling.Types

hundredsMap :: HashMap Text Integer
hundredsMap = HashMap.fromList
  [ ( "y\x00fcz", 100)
  , ( "ikiy\x00fcz", 200)
  , ( "\x00fc\x00e7y\x00fcz", 300)
  , ( "d\x00f6rty\x00fcz", 400)
  , ( "be\x015fy\x00fcz", 500)
  , ( "alt\x0131y\x00fcz", 600)
  , ( "yediy\x00fcz", 700)
  , ( "sekizy\x00fcz", 800)
  , ( "dokuzy\x00fcz", 900)
  ]


ruleInteger5 :: Rule
ruleInteger5 = Rule
  { name = "integer 100..900"
  , pattern =
    [ regex "(y\x00fcz|ikiy\x00fcz|\x00fc\x00e7y\x00fcz|d\x00f6rty\x00fcz|be\x015fy\x00fcz|alt\x0131y\x00fcz|yediy\x00fcz|sekizy\x00fcz|dokuzy\x00fcz)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) hundredsMap >>= integer >>= withGrain 2
      _ -> Nothing
  }

ruleNumeralsPrefixWithNegativeOrMinus :: Rule
ruleNumeralsPrefixWithNegativeOrMinus = Rule
  { name = "numbers prefix with -, negative or minus"
  , pattern =
    [ regex "-|eksi\\s?|negatif\\s?"
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral (NumeralData {TNumeral.value = v}):_) ->
        double (v * (- 1))
      _ -> Nothing
  }

ruleIntegerNumeric :: Rule
ruleIntegerNumeric = Rule
  { name = "integer (numeric)"
  , pattern =
    [ regex "(\\d{1,18})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        v <- parseInt match
        integer $ toInteger v
      _ -> Nothing
  }

ruleACoupleOf :: Rule
ruleACoupleOf = Rule
  { name = "a couple (of)"
  , pattern =
    [ regex "(bir )?\x00e7ift"
    ]
  , prod = \_ -> integer 2 >>= withGrain 1
  }

ruleFew :: Rule
ruleFew = Rule
  { name = "few"
  , pattern =
    [ regex "(bir)?az"
    ]
  , prod = \_ -> integer 3
  }

ruleTen :: Rule
ruleTen = Rule
  { name = "ten"
  , pattern =
    [ regex "on"
    ]
  , prod = \_ -> integer 10 >>= withGrain 1 >>= withMultipliable
  }

ruleDecimalWithThousandsSeparator :: Rule
ruleDecimalWithThousandsSeparator = Rule
  { name = "decimal with thousands separator"
  , pattern =
    [ regex "(\\d+(\\.\\d\\d\\d)+,\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        let dot = Text.singleton '.'
            comma = Text.singleton ','
            fmt = Text.replace comma dot $ Text.replace dot Text.empty match
        in parseDouble fmt >>= double
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
      (Token Numeral (NumeralData {TNumeral.value = val1}):
       Token Numeral (NumeralData {TNumeral.value = val2, TNumeral.grain = g2}):
       _) | isNothing g2 || (isJust g2 && val2 > val1) -> case g2 of
         Nothing -> double $ val1 * val2
         Just g  -> double (val1 * val2) >>= withGrain g
      _ -> Nothing
  }

ruleDecimalNumeral :: Rule
ruleDecimalNumeral = Rule
  { name = "decimal number"
  , pattern =
    [ regex "(\\d*,\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDecimal False match
      _ -> Nothing
  }

numeralSuffixesHalfsuffixTextMap :: HashMap Text Double
numeralSuffixesHalfsuffixTextMap = HashMap.fromList
  [ ( "birbu\x00e7uk", 1.5)
  , ( "bibu\x00e7uk", 1.5)
  , ( "ikibu\x00e7uk", 2.5)
  , ( "\x00fc\231bu\x00e7uk", 3.5)
  , ( "d\x00f6rtbu\x00e7uk", 4.5)
  , ( "be\351bu\x00e7uk", 5.5)
  , ( "alt\x0131bu\x00e7uk", 6.5)
  , ( "yedibu\x00e7uk", 7.5)
  , ( "sekizbu\x00e7uk", 8.5)
  , ( "dokuzbu\x00e7uk", 9.5)
  ]

ruleNumeralSuffixesHalfsuffixText :: Rule
ruleNumeralSuffixesHalfsuffixText = Rule
  { name = "number suffixes (half-suffix text) (1..9)"
  , pattern =
    [ regex "((bir?|iki|\x00fc\x00e7|d\x00f6rt|be\x015f|alt\x0131|yedi|sekiz|dokuz)(bu\x00e7uk))"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) numeralSuffixesHalfsuffixTextMap >>= double
      _ -> Nothing
  }

tenToNintynineMap :: HashMap Text Integer
tenToNintynineMap = HashMap.fromList
  [ ( "onbi", 11)
  , ( "onbir", 11)
  , ( "oniki", 12)
  , ( "on\x00fc\x00e7", 13)
  , ( "ond\x00f6rt", 14)
  , ( "onbe\x015f", 15)
  , ( "onalt\x0131", 16)
  , ( "onyedi", 17)
  , ( "onsekiz", 18)
  , ( "ondokuz", 19)
  , ( "yirmibi", 21)
  , ( "yirmibir", 21)
  , ( "yirmiiki", 22)
  , ( "yirmi\x00fc\x00e7", 23)
  , ( "yirmid\x00f6rt", 24)
  , ( "yirmibe\x015f", 25)
  , ( "yirmialt\x0131", 26)
  , ( "yirmiyedi", 27)
  , ( "yirmisekiz", 28)
  , ( "yirmidokuz", 29)
  , ( "otuzbi", 31)
  , ( "otuzbir", 31)
  , ( "otuziki", 32)
  , ( "otuz\x00fc\x00e7", 33)
  , ( "otuzd\x00f6rt", 34)
  , ( "otuzbe\x015f", 35)
  , ( "otuzalt\x0131", 36)
  , ( "otuzyedi", 37)
  , ( "otuzsekiz", 38)
  , ( "otuzdokuz", 39)
  , ( "k\x0131rkbir", 41)
  , ( "k\x0131rkbi", 41)
  , ( "k\x0131rkiki", 42)
  , ( "k\x0131rk\x00fc\x00e7", 43)
  , ( "k\x0131rkd\x00f6rt", 44)
  , ( "k\x0131rkbe\x015f", 45)
  , ( "k\x0131rkalt\x0131", 46)
  , ( "k\x0131rkyedi", 47)
  , ( "k\x0131rksekiz", 48)
  , ( "k\x0131rkdokuz", 49)
  , ( "ellibi", 51)
  , ( "ellibir", 51)
  , ( "elliiki", 52)
  , ( "elli\x00fc\x00e7", 53)
  , ( "ellid\x00f6rt", 54)
  , ( "ellibe\x015f", 55)
  , ( "ellialt\x0131", 56)
  , ( "elliyedi", 57)
  , ( "ellisekiz", 58)
  , ( "ellidokuz", 59)
  , ( "altm\x0131\x015fbir", 61)
  , ( "atm\x0131\x015fbir", 61)
  , ( "atm\x0131\x015fiki", 62)
  , ( "altm\x0131\x015fiki", 62)
  , ( "atm\x0131\x015f\x00fc\x00e7", 63)
  , ( "altm\x0131\x015f\x00fc\x00e7", 63)
  , ( "atm\x0131\x015fd\x00f6rt", 64)
  , ( "altm\x0131\x015fd\x00f6rt", 64)
  , ( "atm\x0131\x015fbe\x015f", 65)
  , ( "altm\x0131\x015fbe\x015f", 65)
  , ( "atm\x0131\x015falt\x0131", 66)
  , ( "altm\x0131\x015falt\x0131", 66)
  , ( "altm\x0131\x015fyedi", 67)
  , ( "atm\x0131\x015fyedi", 67)
  , ( "altm\x0131\x015fsekiz", 68)
  , ( "atm\x0131\x015fsekiz", 68)
  , ( "atm\x0131\x015fdokuz", 69)
  , ( "altm\x0131\x015fdokuz", 69)
  , ( "yetmi\x015fbir", 71)
  , ( "yetmi\x015fbi", 71)
  , ( "yetmi\x015fiki", 72)
  , ( "yetmi\x015f\x00fc\x00e7", 73)
  , ( "yetmi\x015fd\x00f6rt", 74)
  , ( "yetmi\x015fbe\x015f", 75)
  , ( "yetmi\x015falt\x0131", 76)
  , ( "yetmi\x015fyedi", 77)
  , ( "yetmi\x015fsekiz", 78)
  , ( "yetmi\x015fdokuz", 79)
  , ( "seksenbir", 81)
  , ( "seksenbi", 81)
  , ( "sekseniki", 82)
  , ( "seksen\x00fc\x00e7", 83)
  , ( "seksend\x00f6rt", 84)
  , ( "seksenbe\x015f", 85)
  , ( "seksenalt\x0131", 86)
  , ( "seksenyedi", 87)
  , ( "seksensekiz", 88)
  , ( "seksendokuz", 89)
  , ( "doksanbi", 91)
  , ( "doksanbir", 91)
  , ( "doksaniki", 92)
  , ( "doksan\x00fc\x00e7", 93)
  , ( "doksand\x00f6rt", 94)
  , ( "doksanbe\x015f", 95)
  , ( "doksanalt\x0131", 96)
  , ( "doksanyedi", 97)
  , ( "doksansekiz", 98)
  , ( "doksandokuz", 99)
  ]

ruleInteger3 :: Rule
ruleInteger3 = Rule
  { name = "integer 11..19 21..29 31..39 41..49 51..59 61..69 71..79 81..89 91..99"
  , pattern =
    [ regex "((on|yirmi|otuz|k\x0131rk|elli|atm\x0131\x015f|altm\x0131\x015f|yetmi\x015f|seksen|doksan)(bir|bi|iki|\x00fc\x00e7|d\x00f6rt|be\x015f|alt\x0131|yedi|sekiz|dokuz))"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) tenToNintynineMap >>= integer
      _ -> Nothing
  }


thousandsMap :: HashMap Text Integer
thousandsMap = HashMap.fromList
  [ ( "bin", 1000)
  , ( "ikibin", 2000)
  , ( "\x00fc\x00e7bin", 3000)
  , ( "d\x00f6rtbin", 4000)
  , ( "be\x015fbin", 5000)
  , ( "alt\x0131bin", 6000)
  , ( "yedibin", 7000)
  , ( "sekizbin", 8000)
  , ( "dokuzbin", 9000)
  ]

ruleInteger6 :: Rule
ruleInteger6 = Rule
  { name = "integer 1000..9000"
  , pattern =
    [ regex "(bin|ikibin|\x00fc\x00e7bin|d\x00f6rtbin|be\x015fbin|alt\x0131bin|yedibin|sekizbin|dokuzbin)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) thousandsMap >>= integer >>= withGrain 3
      _ -> Nothing
  }

ruleIntersect :: Rule
ruleIntersect = Rule
  { name = "intersect"
  , pattern =
    [ numberWith (fromMaybe 0 . TNumeral.grain) (>1)
    , numberWith TNumeral.multipliable not
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = val1, TNumeral.grain = Just g}):
       Token Numeral (NumeralData {TNumeral.value = val2}):
       _) | (10 ** fromIntegral g) > val2 -> double $ val1 + val2
      _ -> Nothing
  }

ruleNumeralsSuffixesKMG :: Rule
ruleNumeralsSuffixesKMG = Rule
  { name = "numbers suffixes (K, M, G)"
  , pattern =
    [ dimension Numeral
    , regex "([kmgb])(?=[\\W\\$\x20ac]|$)"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v}):
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
          "k" -> double $ v * 1e3
          "b" -> double $ v * 1e3
          "m" -> double $ v * 1e6
          "g" -> double $ v * 1e9
          _ -> Nothing
      _ -> Nothing
  }

rulePowersOfTen :: Rule
rulePowersOfTen = Rule
  { name = "powers of tens"
  , pattern =
    [ regex "(y(\x00fc)z|bin|milyon)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "y\x00fcz" -> double 1e2 >>= withGrain 2 >>= withMultipliable
        "bin"      -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "milyon"   -> double 1e6 >>= withGrain 6 >>= withMultipliable
        _          -> Nothing
      _ -> Nothing
  }

tenThousandsMap :: HashMap Text Integer
tenThousandsMap = HashMap.fromList
  [  ( "onbin", 10000)
   , ( "yirmibin", 20000)
   , ( "otuzbin", 30000)
   , ( "k\x0131rkbin", 40000)
   , ( "ellibin", 50000)
   , ( "altm\x0131\x015fbin", 60000)
   , ( "atm\x0131\x015fbin", 60000)
   , ( "yetmi\x015fbin", 70000)
   , ( "seksenbin", 80000)
   , ( "doksanbin", 90000)
  ]

ruleInteger7 :: Rule
ruleInteger7 = Rule
  { name = "integer 10000..90000"
  , pattern =
    [ regex "(onbin|yirmibin|otuzbin|k\x0131rkbin|ellibin|atm\x0131\x015fbin|altm\x0131\x015fbin|yetmi\x015fbin|seksenbin|doksanbin)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) tenThousandsMap >>= integer >>= withGrain 4
      _ -> Nothing
  }

hundredThousandsMap :: HashMap Text Integer
hundredThousandsMap = HashMap.fromList
  [ ( "y\x00fczbin", 100000)
  , ( "ikiy\x00fczbin", 200000)
  , ( "\x00fc\x00e7y\x00fczbin", 300000)
  , ( "d\x00f6rty\x00fczbin", 400000)
  , ( "be\x015fy\x00fczbin", 500000)
  , ( "alt\x0131y\x00fczbin", 600000)
  , ( "yediy\x00fczbin", 700000)
  , ( "sekizy\x00fczbin", 800000)
  , ( "dokuzy\x00fczbin", 900000)
  ]

ruleInteger8 :: Rule
ruleInteger8 = Rule
  { name = "integer 100000..900000"
  , pattern =
    [ regex "(y\x00fczbin|ikiy\x00fczbin|\x00fc\x00e7y\x00fczbin|d\x00f6rty\x00fczbin|be\x015fy\x00fczbin|alt\x0131y\x00fczbin|yediy\x00fczbin|sekizy\x00fczbin|dokuzy\x00fczbin)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) hundredThousandsMap >>= integer >>= withGrain 5
      _ -> Nothing
  }

ruleHalf :: Rule
ruleHalf = Rule
  { name = "half"
  , pattern =
    [ regex "(yar\x0131m)"
    ]
  , prod = \_ -> double 0.5
  }

integer9Map :: HashMap Text Double
integer9Map = HashMap.fromList
  [ ( "onbirbu\x00e7uk", 11.5)
  , ( "onbibu\x00e7uk", 11.5)
  , ( "onikibu\x00e7uk", 12.5)
  , ( "on\x00fc\x00e7bu\x00e7uk", 13.5)
  , ( "ond\x00f6rtbu\x00e7uk", 14.5)
  , ( "onbe\x015fbu\x00e7uk", 15.5)
  , ( "onalt\x0131bu\x00e7uk", 16.5)
  , ( "onyedibu\x00e7uk", 17.5)
  , ( "onsekizbu\x00e7uk", 18.5)
  , ( "ondokuzbu\x00e7uk", 19.5)
  , ( "yirmibibu\x00e7uk", 21.5)
  , ( "yirmibirbu\x00e7uk", 21.5)
  , ( "yirmiikibu\x00e7uk", 22.5)
  , ( "yirmi\x00fc\x00e7bu\x00e7uk", 23.5)
  , ( "yirmid\x00f6rtbu\x00e7uk", 24.5)
  , ( "yirmibe\x015fbu\x00e7uk", 25.5)
  , ( "yirmialt\x0131bu\x00e7uk", 26.5)
  , ( "yirmiyedibu\x00e7uk", 27.5)
  , ( "yirmisekizbu\x00e7uk", 28.5)
  , ( "yirmidokuzbu\x00e7uk", 29.5)
  , ( "otuzbibu\x00e7uk", 31.5)
  , ( "otuzbirbu\x00e7uk", 31.5)
  , ( "otuzikibu\x00e7uk", 32.5)
  , ( "otuz\x00fc\x00e7bu\x00e7uk", 33.5)
  , ( "otuzd\x00f6rtbu\x00e7uk", 34.5)
  , ( "otuzbe\x015fbu\x00e7uk", 35.5)
  , ( "otuzalt\x0131bu\x00e7uk", 36.5)
  , ( "otuzyedibu\x00e7uk", 37.5)
  , ( "otuzsekizbu\x00e7uk", 38.5)
  , ( "otuzdokuzbu\x00e7uk", 39.5)
  , ( "k\x0131rkbirbu\x00e7uk", 41.5)
  , ( "k\x0131rkbibu\x00e7uk", 41.5)
  , ( "k\x0131rkikibu\x00e7uk", 42.5)
  , ( "k\x0131rk\x00fc\x00e7bu\x00e7uk", 43.5)
  , ( "k\x0131rkd\x00f6rtbu\x00e7uk", 44.5)
  , ( "k\x0131rkbe\x015fbu\x00e7uk", 45.5)
  , ( "k\x0131rkalt\x0131bu\x00e7uk", 46.5)
  , ( "k\x0131rkyedibu\x00e7uk", 47.5)
  , ( "k\x0131rksekizbu\x00e7uk", 48.5)
  , ( "k\x0131rkdokuzbu\x00e7uk", 49.5)
  , ( "ellibibu\x00e7uk", 51.5)
  , ( "ellibirbu\x00e7uk", 51.5)
  , ( "elliikibu\x00e7uk", 52.5)
  , ( "elli\x00fc\x00e7bu\x00e7uk", 53.5)
  , ( "ellid\x00f6rtbu\x00e7uk", 54.5)
  , ( "ellibe\x015fbu\x00e7uk", 55.5)
  , ( "ellialt\x0131bu\x00e7uk", 56.5)
  , ( "elliyedibu\x00e7uk", 57.5)
  , ( "ellisekizbu\x00e7uk", 58.5)
  , ( "ellidokuzbu\x00e7uk", 59.5)
  , ( "altm\x0131\x015fbirbu\x00e7uk", 61.5)
  , ( "atm\x0131\x015fbirbu\x00e7uk", 61.5)
  , ( "altm\x0131\x015fikibu\x00e7uk", 62.5)
  , ( "atm\x0131\x015fikibu\x00e7uk", 62.5)
  , ( "atm\x0131\x015f\x00fc\x00e7bu\x00e7uk", 63.5)
  , ( "altm\x0131\x015f\x00fc\x00e7bu\x00e7uk", 63.5)
  , ( "altm\x0131\x015fd\x00f6rtbu\x00e7uk", 64.5)
  , ( "atm\x0131\x015fd\x00f6rtbu\x00e7uk", 64.5)
  , ( "altm\x0131\x015fbe\x015fbu\x00e7uk", 65.5)
  , ( "atm\x0131\x015fbe\x015fbu\x00e7uk", 65.5)
  , ( "altm\x0131\x015falt\x0131bu\x00e7uk", 66.5)
  , ( "atm\x0131\x015falt\x0131bu\x00e7uk", 66.5)
  , ( "atm\x0131\x015fyedibu\x00e7uk", 67.5)
  , ( "altm\x0131\x015fyedibu\x00e7uk", 67.5)
  , ( "altm\x0131\x015fsekizbu\x00e7uk", 68.5)
  , ( "atm\x0131\x015fsekizbu\x00e7uk", 68.5)
  , ( "altm\x0131\x015fdokuzbu\x00e7uk", 69.5)
  , ( "atm\x0131\x015fdokuzbu\x00e7uk", 69.5)
  , ( "yetmi\x015fbibu\x00e7uk", 71.5)
  , ( "yetmi\x015fbirbu\x00e7uk", 71.5)
  , ( "yetmi\x015fikibu\x00e7uk", 72.5)
  , ( "yetmi\x015f\x00fc\x00e7bu\x00e7uk", 73.5)
  , ( "yetmi\x015fd\x00f6rtbu\x00e7uk", 74.5)
  , ( "yetmi\x015fbe\x015fbu\x00e7uk", 75.5)
  , ( "yetmi\x015falt\x0131bu\x00e7uk", 76.5)
  , ( "yetmi\x015fyedibu\x00e7uk", 77.5)
  , ( "yetmi\x015fsekizbu\x00e7uk", 78.5)
  , ( "yetmi\x015fdokuzbu\x00e7uk", 79.5)
  , ( "seksenbibu\x00e7uk", 81.5)
  , ( "seksenbirbu\x00e7uk", 81.5)
  , ( "seksenikibu\x00e7uk", 82.5)
  , ( "seksen\x00fc\x00e7bu\x00e7uk", 83.5)
  , ( "seksend\x00f6rtbu\x00e7uk", 84.5)
  , ( "seksenbe\x015fbu\x00e7uk", 85.5)
  , ( "seksenalt\x0131bu\x00e7uk", 86.5)
  , ( "seksenyedibu\x00e7uk", 87.5)
  , ( "seksensekizbu\x00e7uk", 88.5)
  , ( "seksendokuzbu\x00e7uk", 89.5)
  , ( "doksanbirbu\x00e7uk", 91.5)
  , ( "doksanbibu\x00e7uk", 91.5)
  , ( "doksanikibu\x00e7uk", 92.5)
  , ( "doksan\x00fc\x00e7bu\x00e7uk", 93.5)
  , ( "doksand\x00f6rtbu\x00e7uk", 94.5)
  , ( "doksanbe\x015fbu\x00e7uk", 95.5)
  , ( "doksanalt\x0131bu\x00e7uk", 96.5)
  , ( "doksanyedibu\x00e7uk", 97.5)
  , ( "doksansekizbu\x00e7uk", 98.5)
  , ( "doksandokuzbu\x00e7uk", 99.5)
  ]

ruleInteger9 :: Rule
ruleInteger9 = Rule
  { name = "integer 11..19 21..29 31..39 41..49 51..59 61..69 71..79 81..89 91..99"
  , pattern =
    [ regex "((on|yirmi|otuz|k\x0131rk|elli|atm\x0131\x015f|altm\x0131\x015f|yetmi\x015f|seksen|doksan)(bir|bi|iki|\x00fc\x00e7|d\x00f6rt|be\x015f|alt\x0131|yedi|sekiz|dokuz)(bu\x00e7uk))"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) integer9Map >>= double
      _ -> Nothing
  }

ruleDozen :: Rule
ruleDozen = Rule
  { name = "dozen"
  , pattern =
    [ regex "d\x00fczine"
    ]
  , prod = \_ -> integer 12 >>= withGrain 1 >>= withMultipliable
  }

oneToNineMap :: HashMap Text Integer
oneToNineMap = HashMap.fromList
  [ ( "s\305f\305r", 0)
  , ( "yok", 0)
  , ( "hi\x00e7", 0)
  , ( "bir", 1)
  , ( "bi", 1)
  , ( "yek", 1)
  , ( "tek", 1)
  , ( "iki", 2)
  , ( "\x00fc\x00e7", 3)
  , ( "d\x00f6rt", 4)
  , ( "be\x015f", 5)
  , ( "alt\x0131", 6)
  , ( "yedi", 7)
  , ( "sekiz", 8)
  , ( "dokuz", 9)
  ]

ruleInteger :: Rule
ruleInteger = Rule
  { name = "integer (0..9)"
  , pattern =
    [ regex "(yok|hi(\x00e7)|s(\x0131)f(\x0131)r|bir?|[ty]ek|iki|(\x00fc)(\x00e7)|d(\x00f6)rt|be(\x015f)|alt(\x0131)|yedi|sekiz|dokuz)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) oneToNineMap >>= integer
      _ -> Nothing
  }

numeralSuffixesHalfsuffixText2Map :: HashMap Text Double
numeralSuffixesHalfsuffixText2Map = HashMap.fromList
  [ ( "onbu\x00e7uk", 10.5)
  , ( "yirmibu\x00e7uk", 20.5)
  , ( "otuzbu\x00e7uk", 30.5)
  , ( "k\x0131rkbu\x00e7uk", 40.5)
  , ( "ellibu\x00e7uk", 50.5)
  , ( "atm\x0131\x015fbu\x00e7uk", 60.5)
  , ( "altm\x0131\x015fbu\x00e7uk", 60.5)
  , ( "yetmi\x015fbu\x00e7uk", 70.5)
  , ( "seksenbu\x00e7uk", 80.5)
  , ( "doksanbu\x00e7uk", 90.5)
  ]

ruleNumeralSuffixesHalfsuffixText2 :: Rule
ruleNumeralSuffixesHalfsuffixText2 = Rule
  { name = "number suffixes (half-suffix text) (10..90)"
  , pattern =
    [ regex "((on|yirmi|otuz|k\x0131rk|elli|atm\x0131\x015f|altm\x0131\x015f|yetmi\x015f|seksen|doksan)(bu\x00e7uk))"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) numeralSuffixesHalfsuffixText2Map
        >>= double
      _ -> Nothing
  }

ruleNumeralSuffixesHalfSuffix :: Rule
ruleNumeralSuffixesHalfSuffix = Rule
  { name = "number suffixes (half-suffix)"
  , pattern =
    [ dimension Numeral
    , regex "(bu\x00e7uk)(?=[\\W\\$\x20ac]|$)"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v}):_) -> double $ v + 0.5
      _ -> Nothing
  }

ruleInteger4 :: Rule
ruleInteger4 = Rule
  { name = "integer 11..99"
  , pattern =
    [ oneOf [70, 20, 60, 50, 40, 90, 30, 10, 80]
    , numberBetween 1 10
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v1}):
       Token Numeral (NumeralData {TNumeral.value = v2}):
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleGroupOfTens :: Rule
ruleGroupOfTens = Rule
  { name = "group of ten(s)"
  , pattern =
    [ regex "deste"
    ]
  , prod = \_ -> integer 10 >>= withGrain 1 >>= withMultipliable
  }

tensMap :: HashMap Text Integer
tensMap = HashMap.fromList
  [ ( "on", 10)
  , ( "yirmi", 20)
  , ( "otuz", 30)
  , ( "k\x0131rk", 40)
  , ( "elli", 50)
  , ( "altm\x0131\x015f", 60)
  , ( "atm\x0131\x015f", 60)
  , ( "yetmi\x015f", 70)
  , ( "seksen", 80)
  , ( "doksan", 90)
  ]

ruleInteger2 :: Rule
ruleInteger2 = Rule
  { name = "integer (10..90)"
  , pattern =
    [ regex "(on|yirmi|otuz|k\x0131rk|elli|atm\x0131\x015f|altm\x0131\x015f|yetmi\x015f|seksen|doksan)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) tensMap >>= integer
      _ -> Nothing
  }

ruleQuarter :: Rule
ruleQuarter = Rule
  { name = "quarter"
  , pattern =
    [ regex "(\x00e7eyrek)"
    ]
  , prod = \_ -> double 0.25
  }

ruleNumeralDotNumeral :: Rule
ruleNumeralDotNumeral = Rule
  { name = "number dot number"
  , pattern =
    [ dimension Numeral
    , regex "nokta|virg\x00fcl"
    , numberWith TNumeral.grain isNothing
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v1}):
       _:
       Token Numeral (NumeralData {TNumeral.value = v2}):
       _) -> double $ v1 + decimalsToDouble v2
      _ -> Nothing
  }

ruleIntegerWithThousandsSeparator :: Rule
ruleIntegerWithThousandsSeparator = Rule
  { name = "integer with thousands separator ."
  , pattern =
    [ regex "(\\d{1,3}(\\.\\d\\d\\d){1,5})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDouble (Text.replace (Text.singleton '.') Text.empty match) >>= double
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleACoupleOf
  , ruleDecimalNumeral
  , ruleDecimalWithThousandsSeparator
  , ruleDozen
  , ruleFew
  , ruleGroupOfTens
  , ruleHalf
  , ruleInteger
  , ruleInteger2
  , ruleInteger3
  , ruleInteger4
  , ruleInteger5
  , ruleInteger6
  , ruleInteger7
  , ruleInteger8
  , ruleInteger9
  , ruleIntegerNumeric
  , ruleIntegerWithThousandsSeparator
  , ruleIntersect
  , ruleNumeralDotNumeral
  , ruleNumeralSuffixesHalfSuffix
  , ruleNumeralSuffixesHalfsuffixText
  , ruleNumeralSuffixesHalfsuffixText2
  , ruleNumeralsPrefixWithNegativeOrMinus
  , ruleNumeralsSuffixesKMG
  , rulePowersOfTen
  , ruleQuarter
  , ruleTen
  , ruleMultiply
  ]
