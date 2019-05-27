-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.ZH.Rules
  ( rules
  ) where

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

ruleInteger :: Rule
ruleInteger = Rule
  { name = "integer (0..10)"
  , pattern =
    [ regex "(〇|零|一|二|两|兩|三|四|五|六|七|八|九|十)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup match integerMap >>= integer
      _ -> Nothing
  }

integerMap :: HashMap.HashMap Text Integer
integerMap = HashMap.fromList
  [ ( "〇", 0 )
  , ( "零", 0 )
  , ( "一", 1 )
  , ( "兩", 2 )
  , ( "两", 2 )
  , ( "二", 2 )
  , ( "三", 3 )
  , ( "四", 4 )
  , ( "五", 5 )
  , ( "六", 6 )
  , ( "七", 7 )
  , ( "八", 8 )
  , ( "九", 9 )
  , ( "十", 10 )
  ]


ruleNumeralsPrefixWithNegativeOrMinus :: Rule
ruleNumeralsPrefixWithNegativeOrMinus = Rule
  { name = "numbers prefix with -, negative or minus"
  , pattern =
    [ regex "-|负|負"
    , Predicate isPositive
    ]
  , prod = \case
      (_:Token Numeral nd:_) -> double (TNumeral.value nd * (-1))
      _ -> Nothing
  }

ruleDecimalWithThousandsSeparator :: Rule
ruleDecimalWithThousandsSeparator = Rule
  { name = "decimal with thousands separator"
  , pattern =
    [ regex "(\\d+(,\\d\\d\\d)+\\.\\d+)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDouble (Text.replace "," Text.empty match) >>= double
      _ -> Nothing
  }

ruleDecimalNumeral :: Rule
ruleDecimalNumeral = Rule
  { name = "decimal number"
  , pattern =
    [ regex "(\\d*\\.\\d+)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) -> parseDecimal True match
      _ -> Nothing
  }

ruleNumeral :: Rule
ruleNumeral = Rule
  { name = "<number>个/個"
  , pattern =
    [ dimension Numeral
    , regex "个|個"
    ]
  , prod = \case
      (token:_) -> Just token
      _ -> Nothing
  }

numeralSuffixList :: [(Text, Maybe Token)]
numeralSuffixList =
  [ ("K", double 1e3 >>= withGrain 3 >>= withMultipliable)
  , ("M", double 1e6 >>= withGrain 6 >>= withMultipliable)
  , ("G", double 1e9 >>= withGrain 9 >>= withMultipliable)
  , ("十", double 1e1 >>= withGrain 1 >>= withMultipliable)
  , ("百", double 1e2 >>= withGrain 2 >>= withMultipliable)
  , ("千", double 1e3 >>= withGrain 3 >>= withMultipliable)
  , ("万", double 1e4 >>= withGrain 4 >>= withMultipliable)
  , ("亿", double 1e8 >>= withGrain 8 >>= withMultipliable)
  ]

ruleNumeralSuffixes :: [Rule]
ruleNumeralSuffixes = uncurry constructNumeralSuffixRule <$> numeralSuffixList
  where
    constructNumeralSuffixRule :: Text -> Maybe Token -> Rule
    constructNumeralSuffixRule suffixName production = Rule
      { name = "number suffix: " `mappend` suffixName
      , pattern =
        [ regex $ Text.unpack suffixName
        ]
      , prod = const production
      }


ruleMultiply :: Rule
ruleMultiply = Rule
  { name = "compose by multiplication"
  , pattern =
    [ dimension Numeral
    , Predicate isMultipliable
    ]
  , prod = \case
      (token1:token2:_) -> multiply token1 token2
      _ -> Nothing
  }

ruleIntegerWithThousandsSeparator :: Rule
ruleIntegerWithThousandsSeparator = Rule
  { name = "integer with thousands separator ,"
  , pattern =
    [ regex "(\\d{1,3}(,\\d\\d\\d){1,5})"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> let fmt = Text.replace "," Text.empty match
        in parseDouble fmt >>= double
      _ -> Nothing
  }

ruleNumeralsIntersectNonconsectiveUnit :: Rule
ruleNumeralsIntersectNonconsectiveUnit = Rule
  { name = "integer with nonconsecutive unit modifiers"
  , pattern =
    [ Predicate isPositive
    , regex "零|〇"
    , Predicate isPositive
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v1}:_:
       Token Numeral NumeralData{TNumeral.value = v2}:_) ->
        sumConnectedNumbers v1 v2 (diffIntegerDigits v1 v2)
        >>= double
      _ -> Nothing
  }
  where
    sumConnectedNumbers :: Double -> Double -> Int -> Maybe Double
    sumConnectedNumbers v1 v2 d
      | d <= 1 = Nothing
      | otherwise = Just $ v1 + v2

ruleNumeralsIntersectConsecutiveUnit :: Rule
ruleNumeralsIntersectConsecutiveUnit = Rule
  { name = "integer with consecutive unit modifiers"
  , pattern =
    [ Predicate isPositive
    , Predicate isPositive
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v1}:
       Token Numeral NumeralData{TNumeral.value = v2}:_) ->
        sumConnectedNumbers v1 v2 (diffIntegerDigits v1 v2)
        >>= double
      _ -> Nothing
  }
  where
    sumConnectedNumbers :: Double -> Double -> Int -> Maybe Double
    sumConnectedNumbers v1 v2 d
      | d == 1 = Just $ v1 + v2
      | otherwise = Nothing

rules :: [Rule]
rules =
  [ ruleDecimalNumeral
  , ruleDecimalWithThousandsSeparator
  , ruleInteger
  , ruleIntegerWithThousandsSeparator
  , ruleNumeral
  , ruleNumeralsIntersectConsecutiveUnit
  , ruleNumeralsIntersectNonconsectiveUnit
  , ruleNumeralsPrefixWithNegativeOrMinus
  , ruleMultiply
  ]
  ++ ruleNumeralSuffixes
