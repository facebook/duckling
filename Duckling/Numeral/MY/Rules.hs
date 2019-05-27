-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.MY.Rules
  ( rules
  ) where

import Data.HashMap.Strict       (HashMap)
import Data.String
import Data.Text                 (Text)
import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Numeral.Types    (NumeralData (..))
import Duckling.Regex.Types
import Duckling.Types
import Prelude
import qualified Data.HashMap.Strict       as HashMap
import qualified Data.Text                 as Text
import qualified Duckling.Numeral.Types    as TNumeral

ruleInteger5 :: Rule
ruleInteger5 = Rule
  { name = "integer (11..99) "
  , pattern =
    [ numberBetween 1 10
    , regex "ဆယ့်"
    , numberBetween 1 10
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v1}:
       _:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v1 + v2 * 10
      _ -> Nothing
  }

integer09Map :: HashMap Text Integer
integer09Map = HashMap.fromList
  [ ( "၀", 0 )
  , ( "၁", 1 )
  , ( "၂", 2 )
  , ( "၃", 3 )
  , ( "၄", 4 )
  , ( "၅", 5 )
  , ( "၆", 6 )
  , ( "၇", 7 )
  , ( "၈", 8 )
  , ( "၉", 9 )
  ]

ruleInteger09 :: Rule
ruleInteger09 = Rule
  { name = "integer (0..9) - numeric"
  , pattern =
    [ regex "(၀|၁|၂|၃|၄|၅|၆|၇|၈|၉)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup match integer09Map >>= integer
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
      (_:Token Numeral NumeralData{TNumeral.value = v}:_) -> double $ v + 10
      _ -> Nothing
  }

integerPaliMap :: HashMap Text Integer
integerPaliMap = HashMap.fromList
  [ ( "ပထမ", 1 )
  , ( "ဒုတိယ", 2 )
  , ( "တတိယ", 3 )
  ]

ruleIntegerPali :: Rule
ruleIntegerPali = Rule
  { name = "integer (1..3) - pali"
  , pattern =
    [ regex "(ပထမ|ဒုတိယ|တတိယ)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup match integerPaliMap >>= integer
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
      (Token Numeral NumeralData{TNumeral.value = v}:_) -> double $ v * 100
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
      (Token Numeral NumeralData{TNumeral.value = v}:_) -> double $ v * 1000
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
      (Token Numeral NumeralData{TNumeral.value = v}:_) -> double $ v * 10000
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
      (Token Numeral NumeralData{TNumeral.value = v}:_) -> double $ v * 10
      _ -> Nothing
  }

integer2Map :: HashMap Text Integer
integer2Map = HashMap.fromList
  [ ( "တစ်", 1 )
  , ( "နှစ်", 2 )
  , ( "သုံး", 3 )
  , ( "လေး", 4 )
  , ( "ငါး", 5 )
  , ( "ခြေါက်", 6 )
  , ( "ခုနှစ်", 7 )
  , ( "ရှစ်", 8 )
  , ( "ကိုး", 9 )
  , ( "တစ်ဆယ်", 10 )
  ]

ruleInteger2 :: Rule
ruleInteger2 = Rule
  { name = "integer (1..10)"
  , pattern =
    [ regex "(တစ်|နှစ်|သုံး|လေး|ငါး|ခြေါက်|ခုနှစ်|ရှစ်|ကိုး|တစ်ဆယ်)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup match integer2Map >>= integer
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
  , ruleInteger09
  , ruleIntegerPali
  ]
