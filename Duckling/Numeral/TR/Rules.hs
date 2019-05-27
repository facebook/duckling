-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.TR.Rules
  ( rules
  ) where

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

hundredsMap :: HashMap Text Integer
hundredsMap = HashMap.fromList
  [ ( "yüz", 100)
  , ( "ikiyüz", 200)
  , ( "üçyüz", 300)
  , ( "dörtyüz", 400)
  , ( "beşyüz", 500)
  , ( "altıyüz", 600)
  , ( "yediyüz", 700)
  , ( "sekizyüz", 800)
  , ( "dokuzyüz", 900)
  ]


ruleInteger5 :: Rule
ruleInteger5 = Rule
  { name = "integer 100..900"
  , pattern =
    [ regex "(yüz|ikiyüz|üçyüz|dörtyüz|beşyüz|altıyüz|yediyüz|sekizyüz|dokuzyüz)"
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
    [ regex "-|eksi|negatif"
    , Predicate isPositive
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral NumeralData{TNumeral.value = v}:_) ->
        double (v * (- 1))
      _ -> Nothing
  }

ruleACoupleOf :: Rule
ruleACoupleOf = Rule
  { name = "a couple (of)"
  , pattern =
    [ regex "(bir )?çift"
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
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> let fmt = Text.replace "," "." $ Text.replace "." Text.empty match
        in parseDouble fmt >>= double
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
      (Token Numeral NumeralData{TNumeral.value = val1}:
       Token Numeral NumeralData{TNumeral.value = val2, TNumeral.grain = g2}:
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
  [ ( "birbuçuk", 1.5)
  , ( "bibuçuk", 1.5)
  , ( "ikibuçuk", 2.5)
  , ( "ü\231buçuk", 3.5)
  , ( "dörtbuçuk", 4.5)
  , ( "be\351buçuk", 5.5)
  , ( "altıbuçuk", 6.5)
  , ( "yedibuçuk", 7.5)
  , ( "sekizbuçuk", 8.5)
  , ( "dokuzbuçuk", 9.5)
  ]

ruleNumeralSuffixesHalfsuffixText :: Rule
ruleNumeralSuffixesHalfsuffixText = Rule
  { name = "number suffixes (half-suffix text) (1..9)"
  , pattern =
    [ regex "((bir?|iki|üç|dört|beş|altı|yedi|sekiz|dokuz)(buçuk))"
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
  , ( "onüç", 13)
  , ( "ondört", 14)
  , ( "onbeş", 15)
  , ( "onaltı", 16)
  , ( "onyedi", 17)
  , ( "onsekiz", 18)
  , ( "ondokuz", 19)
  , ( "yirmibi", 21)
  , ( "yirmibir", 21)
  , ( "yirmiiki", 22)
  , ( "yirmiüç", 23)
  , ( "yirmidört", 24)
  , ( "yirmibeş", 25)
  , ( "yirmialtı", 26)
  , ( "yirmiyedi", 27)
  , ( "yirmisekiz", 28)
  , ( "yirmidokuz", 29)
  , ( "otuzbi", 31)
  , ( "otuzbir", 31)
  , ( "otuziki", 32)
  , ( "otuzüç", 33)
  , ( "otuzdört", 34)
  , ( "otuzbeş", 35)
  , ( "otuzaltı", 36)
  , ( "otuzyedi", 37)
  , ( "otuzsekiz", 38)
  , ( "otuzdokuz", 39)
  , ( "kırkbir", 41)
  , ( "kırkbi", 41)
  , ( "kırkiki", 42)
  , ( "kırküç", 43)
  , ( "kırkdört", 44)
  , ( "kırkbeş", 45)
  , ( "kırkaltı", 46)
  , ( "kırkyedi", 47)
  , ( "kırksekiz", 48)
  , ( "kırkdokuz", 49)
  , ( "ellibi", 51)
  , ( "ellibir", 51)
  , ( "elliiki", 52)
  , ( "elliüç", 53)
  , ( "ellidört", 54)
  , ( "ellibeş", 55)
  , ( "ellialtı", 56)
  , ( "elliyedi", 57)
  , ( "ellisekiz", 58)
  , ( "ellidokuz", 59)
  , ( "altmışbir", 61)
  , ( "atmışbir", 61)
  , ( "atmışiki", 62)
  , ( "altmışiki", 62)
  , ( "atmışüç", 63)
  , ( "altmışüç", 63)
  , ( "atmışdört", 64)
  , ( "altmışdört", 64)
  , ( "atmışbeş", 65)
  , ( "altmışbeş", 65)
  , ( "atmışaltı", 66)
  , ( "altmışaltı", 66)
  , ( "altmışyedi", 67)
  , ( "atmışyedi", 67)
  , ( "altmışsekiz", 68)
  , ( "atmışsekiz", 68)
  , ( "atmışdokuz", 69)
  , ( "altmışdokuz", 69)
  , ( "yetmişbir", 71)
  , ( "yetmişbi", 71)
  , ( "yetmişiki", 72)
  , ( "yetmişüç", 73)
  , ( "yetmişdört", 74)
  , ( "yetmişbeş", 75)
  , ( "yetmişaltı", 76)
  , ( "yetmişyedi", 77)
  , ( "yetmişsekiz", 78)
  , ( "yetmişdokuz", 79)
  , ( "seksenbir", 81)
  , ( "seksenbi", 81)
  , ( "sekseniki", 82)
  , ( "seksenüç", 83)
  , ( "seksendört", 84)
  , ( "seksenbeş", 85)
  , ( "seksenaltı", 86)
  , ( "seksenyedi", 87)
  , ( "seksensekiz", 88)
  , ( "seksendokuz", 89)
  , ( "doksanbi", 91)
  , ( "doksanbir", 91)
  , ( "doksaniki", 92)
  , ( "doksanüç", 93)
  , ( "doksandört", 94)
  , ( "doksanbeş", 95)
  , ( "doksanaltı", 96)
  , ( "doksanyedi", 97)
  , ( "doksansekiz", 98)
  , ( "doksandokuz", 99)
  ]

ruleInteger3 :: Rule
ruleInteger3 = Rule
  { name = "integer 11..19 21..29 31..39 41..49 51..59 61..69 71..79 81..89 91..99"
  , pattern =
    [ regex "((on|yirmi|otuz|kırk|elli|atmış|altmış|yetmiş|seksen|doksan)(bir|bi|iki|üç|dört|beş|altı|yedi|sekiz|dokuz))"
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
  , ( "üçbin", 3000)
  , ( "dörtbin", 4000)
  , ( "beşbin", 5000)
  , ( "altıbin", 6000)
  , ( "yedibin", 7000)
  , ( "sekizbin", 8000)
  , ( "dokuzbin", 9000)
  ]

ruleInteger6 :: Rule
ruleInteger6 = Rule
  { name = "integer 1000..9000"
  , pattern =
    [ regex "(bin|ikibin|üçbin|dörtbin|beşbin|altıbin|yedibin|sekizbin|dokuzbin)"
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
    [ Predicate hasGrain
    , Predicate $ and . sequence [not . isMultipliable, isPositive]
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = val1, TNumeral.grain = Just g}:
       Token Numeral NumeralData{TNumeral.value = val2}:
       _) | (10 ** fromIntegral g) > val2 -> double $ val1 + val2
      _ -> Nothing
  }

ruleNumeralsSuffixesKMG :: Rule
ruleNumeralsSuffixesKMG = Rule
  { name = "numbers suffixes (K, M, G)"
  , pattern =
    [ dimension Numeral
    , regex "([kmgb])(?=[\\W\\$€]|$)"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:
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
    [ regex "(y(ü)z|bin|milyon)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "yüz" -> double 1e2 >>= withGrain 2 >>= withMultipliable
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
   , ( "kırkbin", 40000)
   , ( "ellibin", 50000)
   , ( "altmışbin", 60000)
   , ( "atmışbin", 60000)
   , ( "yetmişbin", 70000)
   , ( "seksenbin", 80000)
   , ( "doksanbin", 90000)
  ]

ruleInteger7 :: Rule
ruleInteger7 = Rule
  { name = "integer 10000..90000"
  , pattern =
    [ regex "(onbin|yirmibin|otuzbin|kırkbin|ellibin|atmışbin|altmışbin|yetmişbin|seksenbin|doksanbin)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) tenThousandsMap >>= integer >>= withGrain 4
      _ -> Nothing
  }

hundredThousandsMap :: HashMap Text Integer
hundredThousandsMap = HashMap.fromList
  [ ( "yüzbin", 100000)
  , ( "ikiyüzbin", 200000)
  , ( "üçyüzbin", 300000)
  , ( "dörtyüzbin", 400000)
  , ( "beşyüzbin", 500000)
  , ( "altıyüzbin", 600000)
  , ( "yediyüzbin", 700000)
  , ( "sekizyüzbin", 800000)
  , ( "dokuzyüzbin", 900000)
  ]

ruleInteger8 :: Rule
ruleInteger8 = Rule
  { name = "integer 100000..900000"
  , pattern =
    [ regex "(yüzbin|ikiyüzbin|üçyüzbin|dörtyüzbin|beşyüzbin|altıyüzbin|yediyüzbin|sekizyüzbin|dokuzyüzbin)"
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
    [ regex "(yarım)"
    ]
  , prod = \_ -> double 0.5
  }

integer9Map :: HashMap Text Double
integer9Map = HashMap.fromList
  [ ( "onbirbuçuk", 11.5)
  , ( "onbibuçuk", 11.5)
  , ( "onikibuçuk", 12.5)
  , ( "onüçbuçuk", 13.5)
  , ( "ondörtbuçuk", 14.5)
  , ( "onbeşbuçuk", 15.5)
  , ( "onaltıbuçuk", 16.5)
  , ( "onyedibuçuk", 17.5)
  , ( "onsekizbuçuk", 18.5)
  , ( "ondokuzbuçuk", 19.5)
  , ( "yirmibibuçuk", 21.5)
  , ( "yirmibirbuçuk", 21.5)
  , ( "yirmiikibuçuk", 22.5)
  , ( "yirmiüçbuçuk", 23.5)
  , ( "yirmidörtbuçuk", 24.5)
  , ( "yirmibeşbuçuk", 25.5)
  , ( "yirmialtıbuçuk", 26.5)
  , ( "yirmiyedibuçuk", 27.5)
  , ( "yirmisekizbuçuk", 28.5)
  , ( "yirmidokuzbuçuk", 29.5)
  , ( "otuzbibuçuk", 31.5)
  , ( "otuzbirbuçuk", 31.5)
  , ( "otuzikibuçuk", 32.5)
  , ( "otuzüçbuçuk", 33.5)
  , ( "otuzdörtbuçuk", 34.5)
  , ( "otuzbeşbuçuk", 35.5)
  , ( "otuzaltıbuçuk", 36.5)
  , ( "otuzyedibuçuk", 37.5)
  , ( "otuzsekizbuçuk", 38.5)
  , ( "otuzdokuzbuçuk", 39.5)
  , ( "kırkbirbuçuk", 41.5)
  , ( "kırkbibuçuk", 41.5)
  , ( "kırkikibuçuk", 42.5)
  , ( "kırküçbuçuk", 43.5)
  , ( "kırkdörtbuçuk", 44.5)
  , ( "kırkbeşbuçuk", 45.5)
  , ( "kırkaltıbuçuk", 46.5)
  , ( "kırkyedibuçuk", 47.5)
  , ( "kırksekizbuçuk", 48.5)
  , ( "kırkdokuzbuçuk", 49.5)
  , ( "ellibibuçuk", 51.5)
  , ( "ellibirbuçuk", 51.5)
  , ( "elliikibuçuk", 52.5)
  , ( "elliüçbuçuk", 53.5)
  , ( "ellidörtbuçuk", 54.5)
  , ( "ellibeşbuçuk", 55.5)
  , ( "ellialtıbuçuk", 56.5)
  , ( "elliyedibuçuk", 57.5)
  , ( "ellisekizbuçuk", 58.5)
  , ( "ellidokuzbuçuk", 59.5)
  , ( "altmışbirbuçuk", 61.5)
  , ( "atmışbirbuçuk", 61.5)
  , ( "altmışikibuçuk", 62.5)
  , ( "atmışikibuçuk", 62.5)
  , ( "atmışüçbuçuk", 63.5)
  , ( "altmışüçbuçuk", 63.5)
  , ( "altmışdörtbuçuk", 64.5)
  , ( "atmışdörtbuçuk", 64.5)
  , ( "altmışbeşbuçuk", 65.5)
  , ( "atmışbeşbuçuk", 65.5)
  , ( "altmışaltıbuçuk", 66.5)
  , ( "atmışaltıbuçuk", 66.5)
  , ( "atmışyedibuçuk", 67.5)
  , ( "altmışyedibuçuk", 67.5)
  , ( "altmışsekizbuçuk", 68.5)
  , ( "atmışsekizbuçuk", 68.5)
  , ( "altmışdokuzbuçuk", 69.5)
  , ( "atmışdokuzbuçuk", 69.5)
  , ( "yetmişbibuçuk", 71.5)
  , ( "yetmişbirbuçuk", 71.5)
  , ( "yetmişikibuçuk", 72.5)
  , ( "yetmişüçbuçuk", 73.5)
  , ( "yetmişdörtbuçuk", 74.5)
  , ( "yetmişbeşbuçuk", 75.5)
  , ( "yetmişaltıbuçuk", 76.5)
  , ( "yetmişyedibuçuk", 77.5)
  , ( "yetmişsekizbuçuk", 78.5)
  , ( "yetmişdokuzbuçuk", 79.5)
  , ( "seksenbibuçuk", 81.5)
  , ( "seksenbirbuçuk", 81.5)
  , ( "seksenikibuçuk", 82.5)
  , ( "seksenüçbuçuk", 83.5)
  , ( "seksendörtbuçuk", 84.5)
  , ( "seksenbeşbuçuk", 85.5)
  , ( "seksenaltıbuçuk", 86.5)
  , ( "seksenyedibuçuk", 87.5)
  , ( "seksensekizbuçuk", 88.5)
  , ( "seksendokuzbuçuk", 89.5)
  , ( "doksanbirbuçuk", 91.5)
  , ( "doksanbibuçuk", 91.5)
  , ( "doksanikibuçuk", 92.5)
  , ( "doksanüçbuçuk", 93.5)
  , ( "doksandörtbuçuk", 94.5)
  , ( "doksanbeşbuçuk", 95.5)
  , ( "doksanaltıbuçuk", 96.5)
  , ( "doksanyedibuçuk", 97.5)
  , ( "doksansekizbuçuk", 98.5)
  , ( "doksandokuzbuçuk", 99.5)
  ]

ruleInteger9 :: Rule
ruleInteger9 = Rule
  { name = "integer 11..19 21..29 31..39 41..49 51..59 61..69 71..79 81..89 91..99"
  , pattern =
    [ regex "((on|yirmi|otuz|kırk|elli|atmış|altmış|yetmiş|seksen|doksan)(bir|bi|iki|üç|dört|beş|altı|yedi|sekiz|dokuz)(buçuk))"
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
    [ regex "düzine"
    ]
  , prod = \_ -> integer 12 >>= withGrain 1 >>= withMultipliable
  }

oneToNineMap :: HashMap Text Integer
oneToNineMap = HashMap.fromList
  [ ( "s\305f\305r", 0)
  , ( "yok", 0)
  , ( "hiç", 0)
  , ( "bir", 1)
  , ( "bi", 1)
  , ( "yek", 1)
  , ( "tek", 1)
  , ( "iki", 2)
  , ( "üç", 3)
  , ( "dört", 4)
  , ( "beş", 5)
  , ( "altı", 6)
  , ( "yedi", 7)
  , ( "sekiz", 8)
  , ( "dokuz", 9)
  ]

ruleInteger :: Rule
ruleInteger = Rule
  { name = "integer (0..9)"
  , pattern =
    [ regex "(yok|hi(ç)|s(ı)f(ı)r|bir?|[ty]ek|iki|(ü)(ç)|d(ö)rt|be(ş)|alt(ı)|yedi|sekiz|dokuz)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) oneToNineMap >>= integer
      _ -> Nothing
  }

numeralSuffixesHalfsuffixText2Map :: HashMap Text Double
numeralSuffixesHalfsuffixText2Map = HashMap.fromList
  [ ( "onbuçuk", 10.5)
  , ( "yirmibuçuk", 20.5)
  , ( "otuzbuçuk", 30.5)
  , ( "kırkbuçuk", 40.5)
  , ( "ellibuçuk", 50.5)
  , ( "atmışbuçuk", 60.5)
  , ( "altmışbuçuk", 60.5)
  , ( "yetmişbuçuk", 70.5)
  , ( "seksenbuçuk", 80.5)
  , ( "doksanbuçuk", 90.5)
  ]

ruleNumeralSuffixesHalfsuffixText2 :: Rule
ruleNumeralSuffixesHalfsuffixText2 = Rule
  { name = "number suffixes (half-suffix text) (10..90)"
  , pattern =
    [ regex "((on|yirmi|otuz|kırk|elli|atmış|altmış|yetmiş|seksen|doksan)(buçuk))"
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
    , regex "(buçuk)(?=[\\W\\$€]|$)"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:_) -> double $ v + 0.5
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
      (Token Numeral NumeralData{TNumeral.value = v1}:
       Token Numeral NumeralData{TNumeral.value = v2}:
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
  , ( "kırk", 40)
  , ( "elli", 50)
  , ( "altmış", 60)
  , ( "atmış", 60)
  , ( "yetmiş", 70)
  , ( "seksen", 80)
  , ( "doksan", 90)
  ]

ruleInteger2 :: Rule
ruleInteger2 = Rule
  { name = "integer (10..90)"
  , pattern =
    [ regex "(on|yirmi|otuz|kırk|elli|atmış|altmış|yetmiş|seksen|doksan)"
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
    [ regex "(çeyrek)"
    ]
  , prod = \_ -> double 0.25
  }

ruleNumeralDotNumeral :: Rule
ruleNumeralDotNumeral = Rule
  { name = "number dot number"
  , pattern =
    [ dimension Numeral
    , regex "nokta|virgül"
    , Predicate $ not . hasGrain
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v1}:
       _:
       Token Numeral NumeralData{TNumeral.value = v2}:
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
        parseDouble (Text.replace "." Text.empty match) >>= double
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
