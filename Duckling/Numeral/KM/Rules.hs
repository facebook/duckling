-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Numeral.KM.Rules
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

ruleNumeralMap :: HashMap Text Integer
ruleNumeralMap = HashMap.fromList
  [ ( "០", 0 )
  , ( "១", 1 )
  , ( "២", 2 )
  , ( "៣", 3 )
  , ( "៤", 4 )
  , ( "៥", 5 )
  , ( "៦", 6 )
  , ( "៧", 7 )
  , ( "៨", 8 )
  , ( "៩", 9 )
  , ( "សូន្យ", 0 )
  , ( "មួយ", 1 )
  , ( "ពីរ", 2 )
  , ( "បី", 3 )
  , ( "បួន", 4 )
  , ( "ប្រាំ", 5 )
  , ( "ប្រាំមួយ", 6 )
  , ( "ប្រាំពីរ", 7 )
  , ( "ប្រាំបី", 8 )
  , ( "ប្រាំបួន", 9 )
  ]

ruleNumeral :: Rule
ruleNumeral = Rule
  { name = "integer (0..9)"
  -- ត្រូវដាក់ពាក្យ ប្រាំមួយ ប្រាំពីរ ប្រាំបី និងប្រាំបួន នៅខាងមុខលេខប្រាំ។ បើមិនដូច្នោះទេ វាមិនអាចផ្គូរផ្គងត្រូវទេ។
  , pattern =
    [ regex "(០|១|២|៣|៤|៥|៦|៧|៨|៩|ប្រាំបួន|ប្រាំបី|ប្រាំពីរ|ប្រាំមួយ|ប្រាំ|បួន|បី|ពីរ|មួយ|សូន្យ)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup match ruleNumeralMap >>= integer
      _ -> Nothing
  }

ruleTensMap :: HashMap Text Integer
ruleTensMap = HashMap.fromList
  -- លេខ ១០ ២០ ៣០ ៤០ ៥០ ៦០ ៧០ ៨០ ៩០ អត់ត្រូវបានបញ្ចូលទេ ព្រោះមិនទាន់អាចរក
  -- វិធីដកលេខ ០ ចេញ នៅពេលផ្គំុជាមួយលេខដទៃ។
  -- ពពួក "សិប" ក៏មិនត្រូវបានបញ្ចូលដែរ ព្រោះមិនទាន់រកឃើញវិធីផ្គូរផ្គងត្រូវ។
  [ ( "ដប់", 10 )
  , ( "ម្ភៃ", 20 )
  , ( "សាម", 30 )
  , ( "សែ", 40 )
  , ( "ហា", 50 )
  , ( "ហុក", 60 )
  , ( "ចិត", 70 )
  , ( "ប៉ែត", 80 )
  , ( "កៅ", 90 )
  ]

ruleTens :: Rule
ruleTens = Rule
  { name = "integer (10..90)"
  , pattern =
    [ regex "(កៅ|ប៉ែត|ចិត|ហុក|ហា|សែ|សាម|ម្ភៃ|ដប់)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup match ruleTensMap >>= integer
      _ -> Nothing
  }

ruleCompositeTens :: Rule
ruleCompositeTens = Rule
  { name = "integer (11..99)"
  , pattern =
    [ oneOf [10, 20 .. 90]
    , oneOf [1 .. 9]
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v1}:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v1 + v2
      _ -> Nothing
  }

rulePowersOfTen :: Rule
rulePowersOfTen = Rule
  { name = "powers of tens"
  , pattern =
    [ regex "(រយ|ពាន់|ម៉ឺន|សែន|លាន)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case match of
        "រយ" -> double 1e2 >>= withGrain 2 >>= withMultipliable
        "ពាន់" -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "ម៉ឺន" -> double 1e4 >>= withGrain 4 >>= withMultipliable
        "សែន" -> double 1e5 >>= withGrain 5 >>= withMultipliable
        "លាន" -> double 1e6 >>= withGrain 6 >>= withMultipliable
        _ -> Nothing
      _ -> Nothing
  }

-- សម្រាប់ផ្គុំពីរលេខ ដែលចាប់ពីខ្ទង់រយទៅ និងខ្ទង់រាយ
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

-- សម្រាប់លេខចាប់ពីខ្ទង់រយទៅ
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
  [ ruleNumeral
  , ruleTens
  , ruleCompositeTens
  , rulePowersOfTen
  , ruleSum
  , ruleMultiply
  ]
