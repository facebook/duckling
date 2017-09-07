-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.MY.Rules
  ( rules ) where

import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import qualified Duckling.Numeral.Types as TNumeral
import Duckling.Regex.Types
import Duckling.Types

ruleInteger5 :: Rule
ruleInteger5 = Rule
  { name = "integer (11..99) "
  , pattern =
    [ numberBetween 1 10
    , regex "ဆယ့်"
    , numberBetween 1 10
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v1}):
       _:
       Token Numeral (NumeralData {TNumeral.value = v2}):
       _) -> double $ v1 + v2 * 10
      _ -> Nothing
  }

ruleIntegerNumeric :: Rule
ruleIntegerNumeric = Rule
  { name = "integer (0..9) - numeric"
  , pattern =
    [ regex "(၀|၁|၂|၃|၄|၅|၆|၇|၈|၉)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case match of
        "၀" -> integer 0
        "၁" -> integer 1
        "၂" -> integer 2
        "၃" -> integer 3
        "၄" -> integer 4
        "၅" -> integer 5
        "၆" -> integer 6
        "၇" -> integer 7
        "၈" -> integer 8
        "၉" -> integer 9
        _ -> Nothing
      _ -> Nothing
  }

ruleInteger3 :: Rule
ruleInteger3 = Rule
  { name = "integer (11..19) "
  , pattern =
    [ regex "ဆယ့်"
    , numberBetween 1 10
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral (NumeralData {TNumeral.value = v}):_) -> double $ v + 10
      _ -> Nothing
  }

ruleIntegerPali :: Rule
ruleIntegerPali = Rule
  { name = "integer (1..3) - pali"
  , pattern =
    [ regex "(ပထမ|ဒုတိယ|တတိယ)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case match of
        "ပထမ" -> integer 1
        "ဒုတိယ" -> integer 2
        "တတိယ" -> integer 3
        _ -> Nothing
      _ -> Nothing
  }

ruleInteger6 :: Rule
ruleInteger6 = Rule
  { name = "integer (100..900)"
  , pattern =
    [ numberBetween 1 10
    , regex "ရာ"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v}):_) -> double $ v * 100
      _ -> Nothing
  }

ruleInteger7 :: Rule
ruleInteger7 = Rule
  { name = "integer (1000..9000)"
  , pattern =
    [ numberBetween 1 10
    , regex "ထောင်"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v}):_) -> double $ v * 1000
      _ -> Nothing
  }

ruleInteger8 :: Rule
ruleInteger8 = Rule
  { name = "integer (10000..90000)"
  , pattern =
    [ numberBetween 1 10
    , regex "သောင်း"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v}):_) -> double $ v * 10000
      _ -> Nothing
  }

ruleInteger :: Rule
ruleInteger = Rule
  { name = "integer 0"
  , pattern =
    [ regex "သုံည|မရှိ"
    ]
  , prod = \_ -> integer 0
  }

ruleInteger4 :: Rule
ruleInteger4 = Rule
  { name = "integer (10..90)"
  , pattern =
    [ numberBetween 1 10
    , regex "ဆယ်"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v}):_) -> double $ v * 10
      _ -> Nothing
  }

ruleInteger2 :: Rule
ruleInteger2 = Rule
  { name = "integer (1..10)"
  , pattern =
    [ regex "(တစ်|နှစ်|သုံး|လေး|ငါး|ခြေါက်|ခုနှစ်|ရှစ်|ကိုး|တစ်ဆယ်)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case match of
        "တစ်" -> integer 1
        "နှစ်" -> integer 2
        "သုံး" -> integer 3
        "လေး" -> integer 4
        "ငါး" -> integer 5
        "ခြေါက်" -> integer 6
        "ခုနှစ်" -> integer 7
        "ရှစ်" -> integer 8
        "ကိုး" -> integer 9
        "တစ်ဆယ်" -> integer 10
        _ -> Nothing
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleInteger
  , ruleInteger2
  , ruleInteger3
  , ruleInteger4
  , ruleInteger5
  , ruleInteger6
  , ruleInteger7
  , ruleInteger8
  , ruleIntegerNumeric
  , ruleIntegerPali
  ]
