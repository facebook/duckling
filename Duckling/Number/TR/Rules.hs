-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Number.TR.Rules
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

ruleInteger5 :: Rule
ruleInteger5 = Rule
  { name = "integer 100..900"
  , pattern =
    [ regex "(y\x00fcz|ikiy\x00fcz|\x00fc\x00e7y\x00fcz|d\x00f6rty\x00fcz|be\x015fy\x00fcz|alt\x0131y\x00fcz|yediy\x00fcz|sekizy\x00fcz|dokuzy\x00fcz)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "y\x00fcz" -> integer 100 >>= withGrain 2
        "ikiy\x00fcz" -> integer 200 >>= withGrain 2
        "\x00fc\x00e7y\x00fcz" -> integer 300 >>= withGrain 2
        "d\x00f6rty\x00fcz" -> integer 400 >>= withGrain 2
        "be\x015fy\x00fcz" -> integer 500 >>= withGrain 2
        "alt\x0131y\x00fcz" -> integer 600 >>= withGrain 2
        "yediy\x00fcz" -> integer 700 >>= withGrain 2
        "sekizy\x00fcz" -> integer 800 >>= withGrain 2
        "dokuzy\x00fcz" -> integer 900 >>= withGrain 2
        _ -> Nothing
      _ -> Nothing
  }

ruleNumbersPrefixWithNegativeOrMinus :: Rule
ruleNumbersPrefixWithNegativeOrMinus = Rule
  { name = "numbers prefix with -, negative or minus"
  , pattern =
    [ regex "-|eksi\\s?|negatif\\s?"
    , dimension DNumber
    ]
  , prod = \tokens -> case tokens of
      (_:Token DNumber (NumberData {TNumber.value = v}):_) -> double (v * (- 1))
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
    [ regex "(\\d+(,\\d\\d\\d)+\\.\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDouble (Text.replace (Text.singleton ',') Text.empty match) >>= double
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
      (Token DNumber (NumberData {TNumber.value = val1}):
       Token DNumber (NumberData {TNumber.value = val2, TNumber.grain = g2}):
       _) | isNothing g2 || (isJust g2 && val2 > val1) -> case g2 of
         Nothing -> double $ val1 * val2
         Just g  -> double (val1 * val2) >>= withGrain g
      _ -> Nothing
  }

ruleDecimalNumber :: Rule
ruleDecimalNumber = Rule
  { name = "decimal number"
  , pattern =
    [ regex "(\\d*\\.\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDecimal True match
      _ -> Nothing
  }

ruleNumberSuffixesHalfsuffixText :: Rule
ruleNumberSuffixesHalfsuffixText = Rule
  { name = "number suffixes (half-suffix text) (1..9)"
  , pattern =
    [ regex "((bir?|iki|\x00fc\x00e7|d\x00f6rt|be\x015f|alt\x0131|yedi|sekiz|dokuz)(bu\x00e7uk))"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "birbu\x00e7uk" -> double 1.5
        "bibu\x00e7uk" -> double 1.5
        "ikibu\x00e7uk" -> double 2.5
        "\x00fc\231bu\x00e7uk" -> double 3.5
        "d\x00f6rtbu\x00e7uk" -> double 4.5
        "be\351bu\x00e7uk" -> double 5.5
        "alt\x0131bu\x00e7uk" -> double 6.5
        "yedibu\x00e7uk" -> double 7.5
        "sekizbu\x00e7uk" -> double 8.5
        "dokuzbu\x00e7uk" -> double 9.5
        _ -> Nothing
      _ -> Nothing
  }

ruleInteger3 :: Rule
ruleInteger3 = Rule
  { name = "integer 11..19 21..29 31..39 41..49 51..59 61..69 71..79 81..89 91..99"
  , pattern =
    [ regex "((on|yirmi|otuz|k\x0131rk|elli|atm\x0131\x015f|altm\x0131\x015f|yetmi\x015f|seksen|doksan)(bir|bi|iki|\x00fc\x00e7|d\x00f6rt|be\x015f|alt\x0131|yedi|sekiz|dokuz))"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "onbi" -> integer 11
        "onbir" -> integer 11
        "oniki" -> integer 12
        "on\x00fc\x00e7" -> integer 13
        "ond\x00f6rt" -> integer 14
        "onbe\x015f" -> integer 15
        "onalt\x0131" -> integer 16
        "onyedi" -> integer 17
        "onsekiz" -> integer 18
        "ondokuz" -> integer 19
        "yirmibi" -> integer 21
        "yirmibir" -> integer 21
        "yirmiiki" -> integer 22
        "yirmi\x00fc\x00e7" -> integer 23
        "yirmid\x00f6rt" -> integer 24
        "yirmibe\x015f" -> integer 25
        "yirmialt\x0131" -> integer 26
        "yirmiyedi" -> integer 27
        "yirmisekiz" -> integer 28
        "yirmidokuz" -> integer 29
        "otuzbi" -> integer 31
        "otuzbir" -> integer 31
        "otuziki" -> integer 32
        "otuz\x00fc\x00e7" -> integer 33
        "otuzd\x00f6rt" -> integer 34
        "otuzbe\x015f" -> integer 35
        "otuzalt\x0131" -> integer 36
        "otuzyedi" -> integer 37
        "otuzsekiz" -> integer 38
        "otuzdokuz" -> integer 39
        "k\x0131rkbir" -> integer 41
        "k\x0131rkbi" -> integer 41
        "k\x0131rkiki" -> integer 42
        "k\x0131rk\x00fc\x00e7" -> integer 43
        "k\x0131rkd\x00f6rt" -> integer 44
        "k\x0131rkbe\x015f" -> integer 45
        "k\x0131rkalt\x0131" -> integer 46
        "k\x0131rkyedi" -> integer 47
        "k\x0131rksekiz" -> integer 48
        "k\x0131rkdokuz" -> integer 49
        "ellibi" -> integer 51
        "ellibir" -> integer 51
        "elliiki" -> integer 52
        "elli\x00fc\x00e7" -> integer 53
        "ellid\x00f6rt" -> integer 54
        "ellibe\x015f" -> integer 55
        "ellialt\x0131" -> integer 56
        "elliyedi" -> integer 57
        "ellisekiz" -> integer 58
        "ellidokuz" -> integer 59
        "altm\x0131\x015fbir" -> integer 61
        "atm\x0131\x015fbir" -> integer 61
        "atm\x0131\x015fiki" -> integer 62
        "altm\x0131\x015fiki" -> integer 62
        "atm\x0131\x015f\x00fc\x00e7" -> integer 63
        "altm\x0131\x015f\x00fc\x00e7" -> integer 63
        "atm\x0131\x015fd\x00f6rt" -> integer 64
        "altm\x0131\x015fd\x00f6rt" -> integer 64
        "atm\x0131\x015fbe\x015f" -> integer 65
        "altm\x0131\x015fbe\x015f" -> integer 65
        "atm\x0131\x015falt\x0131" -> integer 66
        "altm\x0131\x015falt\x0131" -> integer 66
        "altm\x0131\x015fyedi" -> integer 67
        "atm\x0131\x015fyedi" -> integer 67
        "altm\x0131\x015fsekiz" -> integer 68
        "atm\x0131\x015fsekiz" -> integer 68
        "atm\x0131\x015fdokuz" -> integer 69
        "altm\x0131\x015fdokuz" -> integer 69
        "yetmi\x015fbir" -> integer 71
        "yetmi\x015fbi" -> integer 71
        "yetmi\x015fiki" -> integer 72
        "yetmi\x015f\x00fc\x00e7" -> integer 73
        "yetmi\x015fd\x00f6rt" -> integer 74
        "yetmi\x015fbe\x015f" -> integer 75
        "yetmi\x015falt\x0131" -> integer 76
        "yetmi\x015fyedi" -> integer 77
        "yetmi\x015fsekiz" -> integer 78
        "yetmi\x015fdokuz" -> integer 79
        "seksenbir" -> integer 81
        "seksenbi" -> integer 81
        "sekseniki" -> integer 82
        "seksen\x00fc\x00e7" -> integer 83
        "seksend\x00f6rt" -> integer 84
        "seksenbe\x015f" -> integer 85
        "seksenalt\x0131" -> integer 86
        "seksenyedi" -> integer 87
        "seksensekiz" -> integer 88
        "seksendokuz" -> integer 89
        "doksanbi" -> integer 91
        "doksanbir" -> integer 91
        "doksaniki" -> integer 92
        "doksan\x00fc\x00e7" -> integer 93
        "doksand\x00f6rt" -> integer 94
        "doksanbe\x015f" -> integer 95
        "doksanalt\x0131" -> integer 96
        "doksanyedi" -> integer 97
        "doksansekiz" -> integer 98
        "doksandokuz" -> integer 99
        _ -> Nothing
      _ -> Nothing
  }

ruleInteger6 :: Rule
ruleInteger6 = Rule
  { name = "integer 1000..9000"
  , pattern =
    [ regex "(bin|ikibin|\x00fc\x00e7bin|d\x00f6rtbin|be\x015fbin|alt\x0131bin|yedibin|sekizbin|dokuzbin)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "bin" -> integer 1000 >>= withGrain 3
        "ikibin" -> integer 2000 >>= withGrain 3
        "\x00fc\x00e7bin" -> integer 3000 >>= withGrain 3
        "d\x00f6rtbin" -> integer 4000 >>= withGrain 3
        "be\x015fbin" -> integer 5000 >>= withGrain 3
        "alt\x0131bin" -> integer 6000 >>= withGrain 3
        "yedibin" -> integer 7000 >>= withGrain 3
        "sekizbin" -> integer 8000 >>= withGrain 3
        "dokuzbin" -> integer 9000 >>= withGrain 3
        _ -> Nothing
      _ -> Nothing
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

ruleNumbersSuffixesKMG :: Rule
ruleNumbersSuffixesKMG = Rule
  { name = "numbers suffixes (K, M, G)"
  , pattern =
    [ dimension DNumber
    , regex "([kmgb])(?=[\\W\\$\x20ac]|$)"
    ]
  , prod = \tokens -> case tokens of
      (Token DNumber (NumberData {TNumber.value = v}):
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

ruleInteger7 :: Rule
ruleInteger7 = Rule
  { name = "integer 10000..90000"
  , pattern =
    [ regex "(onbin|yirmibin|otuzbin|k\x0131rkbin|ellibin|atm\x0131\x015fbin|altm\x0131\x015fbin|yetmi\x015fbin|seksenbin|doksanbin)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "onbin" -> integer 10000 >>= withGrain 4
        "yirmibin" -> integer 20000 >>= withGrain 4
        "otuzbin" -> integer 30000 >>= withGrain 4
        "k\x0131rkbin" -> integer 40000 >>= withGrain 4
        "ellibin" -> integer 50000 >>= withGrain 4
        "altm\x0131\x015fbin" -> integer 60000 >>= withGrain 4
        "atm\x0131\x015fbin" -> integer 60000 >>= withGrain 4
        "yetmi\x015fbin" -> integer 70000 >>= withGrain 4
        "seksenbin" -> integer 80000 >>= withGrain 4
        "doksanbin" -> integer 90000 >>= withGrain 4
        _ -> Nothing
      _ -> Nothing
  }

ruleInteger8 :: Rule
ruleInteger8 = Rule
  { name = "integer 100000..900000"
  , pattern =
    [ regex "(y\x00fczbin|ikiy\x00fczbin|\x00fc\x00e7y\x00fczbin|d\x00f6rty\x00fczbin|be\x015fy\x00fczbin|alt\x0131y\x00fczbin|yediy\x00fczbin|sekizy\x00fczbin|dokuzy\x00fczbin)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "y\x00fczbin" -> integer 100000 >>= withGrain 5
        "ikiy\x00fczbin" -> integer 200000 >>= withGrain 5
        "\x00fc\x00e7y\x00fczbin" -> integer 300000 >>= withGrain 5
        "d\x00f6rty\x00fczbin" -> integer 400000 >>= withGrain 5
        "be\x015fy\x00fczbin" -> integer 500000 >>= withGrain 5
        "alt\x0131y\x00fczbin" -> integer 600000 >>= withGrain 5
        "yediy\x00fczbin" -> integer 700000 >>= withGrain 5
        "sekizy\x00fczbin" -> integer 800000 >>= withGrain 5
        "dokuzy\x00fczbin" -> integer 900000 >>= withGrain 5
        _ -> Nothing
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

ruleInteger9 :: Rule
ruleInteger9 = Rule
  { name = "integer 11..19 21..29 31..39 41..49 51..59 61..69 71..79 81..89 91..99"
  , pattern =
    [ regex "((on|yirmi|otuz|k\x0131rk|elli|atm\x0131\x015f|altm\x0131\x015f|yetmi\x015f|seksen|doksan)(bir|bi|iki|\x00fc\x00e7|d\x00f6rt|be\x015f|alt\x0131|yedi|sekiz|dokuz)(bu\x00e7uk))"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "onbirbu\x00e7uk" -> double 11.5
        "onbibu\x00e7uk" -> double 11.5
        "onikibu\x00e7uk" -> double 12.5
        "on\x00fc\x00e7bu\x00e7uk" -> double 13.5
        "ond\x00f6rtbu\x00e7uk" -> double 14.5
        "onbe\x015fbu\x00e7uk" -> double 15.5
        "onalt\x0131bu\x00e7uk" -> double 16.5
        "onyedibu\x00e7uk" -> double 17.5
        "onsekizbu\x00e7uk" -> double 18.5
        "ondokuzbu\x00e7uk" -> double 19.5
        "yirmibibu\x00e7uk" -> double 21.5
        "yirmibirbu\x00e7uk" -> double 21.5
        "yirmiikibu\x00e7uk" -> double 22.5
        "yirmi\x00fc\x00e7bu\x00e7uk" -> double 23.5
        "yirmid\x00f6rtbu\x00e7uk" -> double 24.5
        "yirmibe\x015fbu\x00e7uk" -> double 25.5
        "yirmialt\x0131bu\x00e7uk" -> double 26.5
        "yirmiyedibu\x00e7uk" -> double 27.5
        "yirmisekizbu\x00e7uk" -> double 28.5
        "yirmidokuzbu\x00e7uk" -> double 29.5
        "otuzbibu\x00e7uk" -> double 31.5
        "otuzbirbu\x00e7uk" -> double 31.5
        "otuzikibu\x00e7uk" -> double 32.5
        "otuz\x00fc\x00e7bu\x00e7uk" -> double 33.5
        "otuzd\x00f6rtbu\x00e7uk" -> double 34
        "otuzbe\x015fbu\x00e7uk" -> double 35.5
        "otuzalt\x0131bu\x00e7uk" -> double 36.5
        "otuzyedibu\x00e7uk" -> double 37.5
        "otuzsekizbu\x00e7uk" -> double 38.5
        "otuzdokuzbu\x00e7uk" -> double 39.5
        "k\x0131rkbirbu\x00e7uk" -> double 41.5
        "k\x0131rkbibu\x00e7uk" -> double 41.5
        "k\x0131rkikibu\x00e7uk" -> double 42.5
        "k\x0131rk\x00fc\x00e7bu\x00e7uk" -> double 43.5
        "k\x0131rkd\x00f6rtbu\x00e7uk" -> double 44
        "k\x0131rkbe\x015fbu\x00e7uk" -> double 45.5
        "k\x0131rkalt\x0131bu\x00e7uk" -> double 46.5
        "k\x0131rkyedibu\x00e7uk" -> double 47.5
        "k\x0131rksekizbu\x00e7uk" -> double 48.5
        "k\x0131rkdokuzbu\x00e7uk" -> double 49.5
        "ellibibu\x00e7uk" -> double 51.5
        "ellibirbu\x00e7uk" -> double 51.5
        "elliikibu\x00e7uk" -> double 52.5
        "elli\x00fc\x00e7bu\x00e7uk" -> double 53.5
        "ellid\x00f6rtbu\x00e7uk" -> double 54
        "ellibe\x015fbu\x00e7uk" -> double 55.5
        "ellialt\x0131bu\x00e7uk" -> double 56.5
        "elliyedibu\x00e7uk" -> double 57.5
        "ellisekizbu\x00e7uk" -> double 58.5
        "ellidokuzbu\x00e7uk" -> double 59.5
        "altm\x0131\x015fbirbu\x00e7uk" -> double 61.5
        "atm\x0131\x015fbirbu\x00e7uk" -> double 61.5
        "altm\x0131\x015fikibu\x00e7uk" -> double 62.5
        "atm\x0131\x015fikibu\x00e7uk" -> double 62.5
        "atm\x0131\x015f\x00fc\x00e7bu\x00e7uk" -> double 63.5
        "altm\x0131\x015f\x00fc\x00e7bu\x00e7uk" -> double 63.5
        "altm\x0131\x015fd\x00f6rtbu\x00e7uk" -> double 64.5
        "atm\x0131\x015fd\x00f6rtbu\x00e7uk" -> double 64.5
        "altm\x0131\x015fbe\x015fbu\x00e7uk" -> double 65.5
        "atm\x0131\x015fbe\x015fbu\x00e7uk" -> double 65.5
        "altm\x0131\x015falt\x0131bu\x00e7uk" -> double 66.5
        "atm\x0131\x015falt\x0131bu\x00e7uk" -> double 66.5
        "atm\x0131\x015fyedibu\x00e7uk" -> double 67.5
        "altm\x0131\x015fyedibu\x00e7uk" -> double 67.5
        "altm\x0131\x015fsekizbu\x00e7uk" -> double 68.5
        "atm\x0131\x015fsekizbu\x00e7uk" -> double 68.5
        "altm\x0131\x015fdokuzbu\x00e7uk" -> double 69.5
        "atm\x0131\x015fdokuzbu\x00e7uk" -> double 69.5
        "yetmi\x015fbibu\x00e7uk" -> double 71.5
        "yetmi\x015fbirbu\x00e7uk" -> double 71.5
        "yetmi\x015fikibu\x00e7uk" -> double 72.5
        "yetmi\x015f\x00fc\x00e7bu\x00e7uk" -> double 73.5
        "yetmi\x015fd\x00f6rtbu\x00e7uk" -> double 74.5
        "yetmi\x015fbe\x015fbu\x00e7uk" -> double 75.5
        "yetmi\x015falt\x0131bu\x00e7uk" -> double 76.5
        "yetmi\x015fyedibu\x00e7uk" -> double 77.5
        "yetmi\x015fsekizbu\x00e7uk" -> double 78.5
        "yetmi\x015fdokuzbu\x00e7uk" -> double 79.5
        "seksenbibu\x00e7uk" -> double 81.5
        "seksenbirbu\x00e7uk" -> double 81.5
        "seksenikibu\x00e7uk" -> double 82.5
        "seksen\x00fc\x00e7bu\x00e7uk" -> double 83.5
        "seksend\x00f6rtbu\x00e7uk" -> double 84.5
        "seksenbe\x015fbu\x00e7uk" -> double 85.5
        "seksenalt\x0131bu\x00e7uk" -> double 86.5
        "seksenyedibu\x00e7uk" -> double 87.5
        "seksensekizbu\x00e7uk" -> double 88.5
        "seksendokuzbu\x00e7uk" -> double 89.5
        "doksanbirbu\x00e7uk" -> double 91.5
        "doksanbibu\x00e7uk" -> double 91.5
        "doksanikibu\x00e7uk" -> double 92.5
        "doksan\x00fc\x00e7bu\x00e7uk" -> double 93.5
        "doksand\x00f6rtbu\x00e7uk" -> double 94.5
        "doksanbe\x015fbu\x00e7uk" -> double 95.5
        "doksanalt\x0131bu\x00e7uk" -> double 96.5
        "doksanyedibu\x00e7uk" -> double 97.5
        "doksansekizbu\x00e7uk" -> double 98.5
        "doksandokuzbu\x00e7uk" -> double 99.5
        _ -> Nothing
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

ruleInteger :: Rule
ruleInteger = Rule
  { name = "integer (0..9)"
  , pattern =
    [ regex "(yok|hi(\x00e7)|s(\x0131)f(\x0131)r|bir?|[ty]ek|iki|(\x00fc)(\x00e7)|d(\x00f6)rt|be(\x015f)|alt(\x0131)|yedi|sekiz|dokuz)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "s\305f\305r" -> integer 0
        "yok" -> integer 0
        "hi\x00e7" -> integer 0
        "bir" -> integer 1
        "bi" -> integer 1
        "yek" -> integer 1
        "tek" -> integer 1
        "iki" -> integer 2
        "\x00fc\x00e7" -> integer 3
        "d\x00f6rt" -> integer 4
        "be\x015f" -> integer 5
        "alt\x0131" -> integer 6
        "yedi" -> integer 7
        "sekiz" -> integer 8
        "dokuz" -> integer 9
        _ -> Nothing
      _ -> Nothing
  }

ruleNumberSuffixesHalfsuffixText2 :: Rule
ruleNumberSuffixesHalfsuffixText2 = Rule
  { name = "number suffixes (half-suffix text) (10..90)"
  , pattern =
    [ regex "((on|yirmi|otuz|k\x0131rk|elli|atm\x0131\x015f|altm\x0131\x015f|yetmi\x015f|seksen|doksan)(bu\x00e7uk))"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "onbu\x00e7uk" -> double 10.5
        "yirmibu\x00e7uk" -> double 20.5
        "otuzbu\x00e7uk" -> double 30.5
        "k\x0131rkbu\x00e7uk" -> double 40.5
        "ellibu\x00e7uk" -> double 50.5
        "atm\x0131\x015fbu\x00e7uk" -> double 60.5
        "altm\x0131\x015fbu\x00e7uk" -> double 60.5
        "yetmi\x015fbu\x00e7uk" -> double 70.5
        "seksenbu\x00e7uk" -> double 80.5
        "doksanbu\x00e7uk" -> double 90.5
        _ -> Nothing
      _ -> Nothing
  }

ruleNumberSuffixesHalfSuffix :: Rule
ruleNumberSuffixesHalfSuffix = Rule
  { name = "number suffixes (half-suffix)"
  , pattern =
    [ dimension DNumber
    , regex "(bu\x00e7uk)(?=[\\W\\$\x20ac]|$)"
    ]
  , prod = \tokens -> case tokens of
      (Token DNumber (NumberData {TNumber.value = v}):_) -> double $ v + 0.5
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
      (Token DNumber (NumberData {TNumber.value = v1}):
       Token DNumber (NumberData {TNumber.value = v2}):
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

ruleInteger2 :: Rule
ruleInteger2 = Rule
  { name = "integer (10..90)"
  , pattern =
    [ regex "(on|yirmi|otuz|k\x0131rk|elli|atm\x0131\x015f|altm\x0131\x015f|yetmi\x015f|seksen|doksan)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "on" -> integer 10
        "yirmi" -> integer 20
        "otuz" -> integer 30
        "k\x0131rk" -> integer 40
        "elli" -> integer 50
        "altm\x0131\x015f" -> integer 60
        "atm\x0131\x015f" -> integer 60
        "yetmi\x015f" -> integer 70
        "seksen" -> integer 80
        "doksan" -> integer 90
        _ -> Nothing
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

ruleNumberDotNumber :: Rule
ruleNumberDotNumber = Rule
  { name = "number dot number"
  , pattern =
    [ dimension DNumber
    , regex "nokta|virg\x00fcl"
    , numberWith TNumber.grain isNothing
    ]
  , prod = \tokens -> case tokens of
      (Token DNumber (NumberData {TNumber.value = v1}):
       _:
       Token DNumber (NumberData {TNumber.value = v2}):
       _) -> double $ v1 + decimalsToDouble v2
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

rules :: [Rule]
rules =
  [ ruleACoupleOf
  , ruleDecimalNumber
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
  , ruleNumberDotNumber
  , ruleNumberSuffixesHalfSuffix
  , ruleNumberSuffixesHalfsuffixText
  , ruleNumberSuffixesHalfsuffixText2
  , ruleNumbersPrefixWithNegativeOrMinus
  , ruleNumbersSuffixesKMG
  , rulePowersOfTen
  , ruleQuarter
  , ruleTen
  , ruleMultiply
  ]
