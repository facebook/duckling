-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Number.UK.Rules
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
  { name = "integer (20..90)"
  , pattern =
    [ regex "(\x0434\x0432\x0430\x0434\x0446\x044f\x0442\x044c|\x0442\x0440\x0438\x0434\x0446\x044f\x0442\x044c|\x0441\x043e\x0440\x043e\x043a|\x043f\x2018\x044f\x0442\x0434\x0435\x0441\x044f\x0442|\x0448\x0456\x0441\x0442\x0434\x0435\x0441\x044f\x0442|\x0441\x0456\x043c\x0434\x0435\x0441\x044f\x0442|\x0432\x0456\x0441\x0456\x043c\x0434\x0435\x0441\x044f\x0442|\x0434\x0435\x0432\x2018\x044f\x043d\x043e\x0441\x0442\x043e)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case match of
        "\x0434\x0432\x0430\x0434\x0446\x044f\x0442\x044c" -> integer 20
        "\x0442\x0440\x0438\x0434\x0446\x044f\x0442\x044c" -> integer 30
        "\x0441\x043e\x0440\x043e\x043a" -> integer 40
        "\x043f\x2018\x044f\x0442\x0434\x0435\x0441\x044f\x0442" -> integer 50
        "\x0448\x0456\x0441\x0442\x0434\x0435\x0441\x044f\x0442" -> integer 60
        "\x0441\x0456\x043c\x0434\x0435\x0441\x044f\x0442" -> integer 70
        "\x0432\x0456\x0441\x0456\x043c\x0434\x0435\x0441\x044f\x0442" -> integer 80
        "\x0434\x0435\x0432\x2018\x044f\x043d\x043e\x0441\x0442\x043e" -> integer 90
        _ -> Nothing
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

ruleInteger3 :: Rule
ruleInteger3 = Rule
  { name = "integer 2"
  , pattern =
    [ regex "(\x0434\x0432\x0430|\x0434\x0432\x0456|\x0434\x0432\x043e\x0454|\x043f\x0430\x0440\x0430|\x043f\x0430\x0440\x0443|\x043f\x0430\x0440\x043e\x0447\x043a\x0443|\x043f\x0430\x0440\x043e\x0447\x043a\x0430)"
    ]
  , prod = \_ -> integer 2
  }

ruleInteger6 :: Rule
ruleInteger6 = Rule
  { name = "integer (100..900)"
  , pattern =
    [ regex "(\x0441\x0442\x043e|\x0434\x0432\x0456\x0441\x0442\x0456|\x0442\x0440\x0438\x0441\x0442\x0430|\x0447\x043e\x0442\x0438\x0440\x0438\x0441\x0442\x0430|\x043f\x2018\x044f\x0442\x0441\x043e\x0442|\x0448\x0456\x0441\x0442\x0441\x043e\x0442|\x0441\x0456\x043c\x0441\x043e\x0442|\x0432\x0456\x0441\x0456\x043c\x0441\x043e\x0442|\x0434\x0435\x0432\x2018\x044f\x0442\x0441\x043e\x0442)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case match of
        "\x0441\x0442\x043e" -> integer 100
        "\x0434\x0432\x0456\x0441\x0442\x0456" -> integer 200
        "\x0442\x0440\x0438\x0441\x0442\x0430" -> integer 300
        "\x0447\x043e\x0442\x0438\x0440\x0438\x0441\x0442\x0430" -> integer 400
        "\x043f\x2018\x044f\x0442\x0441\x043e\x0442" -> integer 500
        "\x0448\x0456\x0441\x0442\x0441\x043e\x0442" -> integer 600
        "\x0441\x0456\x043c\x0441\x043e\x0442" -> integer 700
        "\x0432\x0456\x0441\x0456\x043c\x0441\x043e\x0442" -> integer 800
        "\x0434\x0435\x0432\x2018\x044f\x0442\x0441\x043e\x0442" -> integer 900
        _ -> Nothing
      _ -> Nothing
  }

ruleNumbersPrefixWithMinus :: Rule
ruleNumbersPrefixWithMinus = Rule
  { name = "numbers prefix with -, minus"
  , pattern =
    [ regex "-|\x043c\x0456\x043d\x0443\x0441\\s?"
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral nd:_) -> double (TNumber.value nd * (-1))
      _ -> Nothing
  }

ruleNumbersSuffixesKMG :: Rule
ruleNumbersSuffixesKMG = Rule
  { name = "numbers suffixes (K, M, G)"
  , pattern =
    [ dimension Numeral
    , regex "((\x043a|\x043c|\x0433)|(\x041a|\x041c|\x0413))(?=[\\W\\$\x20ac]|$)"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumberData {TNumber.value = v}):
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
         "\x043a" -> double $ v * 1e3
         "\x041a" -> double $ v * 1e3
         "\x043c" -> double $ v * 1e6
         "\x041c" -> double $ v * 1e6
         "\x0433" -> double $ v * 1e9
         "\x0413" -> double $ v * 1e9
         _   -> Nothing
      _ -> Nothing
  }

ruleInteger7 :: Rule
ruleInteger7 = Rule
  { name = "integer 21..99"
  , pattern =
    [ oneOf [70, 20, 60, 50, 40, 90, 30, 80]
    , numberBetween 1 10
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumberData {TNumber.value = v1}):
       Token Numeral (NumberData {TNumber.value = v2}):
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleInteger8 :: Rule
ruleInteger8 = Rule
  { name = "integer 101..999"
  , pattern =
    [ oneOf [300, 600, 500, 100, 800, 200, 900, 700, 400]
    , numberBetween 1 100
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumberData {TNumber.value = v1}):
       Token Numeral (NumberData {TNumber.value = v2}):
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleInteger :: Rule
ruleInteger = Rule
  { name = "integer 0"
  , pattern =
    [ regex "(\x043d\x0443\x043b\x044c)"
    ]
  , prod = \_ -> integer 0
  }

ruleInteger4 :: Rule
ruleInteger4 = Rule
  { name = "integer (3..19)"
  , pattern =
    [ regex "(\x0442\x0440\x0438|\x0447\x043e\x0442\x0438\x0440\x043d\x0430\x0434\x0446\x044f\x0442\x044c|\x0447\x043e\x0442\x0438\x0440\x0438|\x043f\x2018\x044f\x0442\x043d\x0430\x0434\x0446\x044f\x0442\x044c|\x043f\x2018\x044f\x0442\x044c|\x0448\x0456\x0441\x0442\x043d\x0430\x0434\x0446\x044f\x0442\x044c|\x0448\x0456\x0441\x0442\x044c|\x0441\x0456\x043c\x043d\x0430\x0434\x0446\x044f\x0442\x044c|\x0441\x0456\x043c|\x0432\x0456\x0441\x0456\x043c\x043d\x0430\x0434\x0446\x044f\x0442\x044c|\x0432\x0456\x0441\x0456\x043c|\x0434\x0435\x0432\x2018\x044f\x0442\x043d\x0430\x0434\x0446\x044f\x0442\x044c|\x0434\x0435\x0432\x2018\x044f\x0442\x044c|\x0434\x0435\x0441\x044f\x0442\x044c|\x043e\x0434\x0438\x043d\x0430\x0434\x0446\x044f\x0442\x044c|\x0434\x0432\x0430\x043d\x0430\x0434\x0446\x044f\x0442\x044c|\x0442\x0440\x0438\x043d\x0430\x0434\x0446\x044f\x0442\x044c)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case match of
        "\x0442\x0440\x0438" -> integer 3
        "\x0447\x043e\x0442\x0438\x0440\x0438" -> integer 4
        "\x043f\x2018\x044f\x0442\x044c" -> integer 5
        "\x0448\x0456\x0441\x0442\x044c" -> integer 6
        "\x0441\x0456\x043c" -> integer 7
        "\x0432\x0456\x0441\x0456\x043c" -> integer 8
        "\x0434\x0435\x0432\x2018\x044f\x0442\x044c" -> integer 9
        "\x0434\x0435\x0441\x044f\x0442\x044c" -> integer 10
        "\x043e\x0434\x0438\x043d\x0430\x0434\x0446\x044f\x0442\x044c" -> integer 11
        "\x0434\x0432\x0430\x043d\x0430\x0434\x0446\x044f\x0442\x044c" -> integer 12
        "\x0442\x0440\x0438\x043d\x0430\x0434\x0446\x044f\x0442\x044c" -> integer 13
        "\x0447\x043e\x0442\x0438\x0440\x043d\x0430\x0434\x0446\x044f\x0442\x044c" -> integer 14
        "\x043f\x2018\x044f\x0442\x043d\x0430\x0434\x0446\x044f\x0442\x044c" -> integer 15
        "\x0448\x0456\x0441\x0442\x043d\x0430\x0434\x0446\x044f\x0442\x044c" -> integer 16
        "\x0441\x0456\x043c\x043d\x0430\x0434\x0446\x044f\x0442\x044c" -> integer 17
        "\x0432\x0456\x0441\x0456\x043c\x043d\x0430\x0434\x0446\x044f\x0442\x044c" -> integer 18
        "\x0434\x0435\x0432\x2018\x044f\x0442\x043d\x0430\x0434\x0446\x044f\x0442\x044c" -> integer 19
        _ -> Nothing
      _ -> Nothing
  }

ruleInteger2 :: Rule
ruleInteger2 = Rule
  { name = "integer 1"
  , pattern =
    [ regex "(\x043e\x0434\x0438\x043d|\x043e\x0434\x043d\x0430|\x043e\x0434\x043d\x0443|\x043e\x0434\x043d\x0435|\x043e\x0434\x043d\x043e\x0433\x043e)"
    ]
  , prod = \_ -> integer 1
  }

ruleNumberDotNumber :: Rule
ruleNumberDotNumber = Rule
  { name = "number dot number"
  , pattern =
    [ dimension Numeral
    , regex "\x043a\x0440\x0430\x043f\x043a\x0430"
    , numberWith TNumber.grain isNothing
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral nd1:_:Token Numeral nd2:_) ->
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
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> let fmt = Text.replace (Text.singleton ',') Text.empty match
        in parseDouble fmt >>= double
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleDecimalNumber
  , ruleDecimalWithThousandsSeparator
  , ruleInteger
  , ruleInteger2
  , ruleInteger3
  , ruleInteger4
  , ruleInteger5
  , ruleInteger6
  , ruleInteger7
  , ruleInteger8
  , ruleIntegerNumeric
  , ruleIntegerWithThousandsSeparator
  , ruleNumberDotNumber
  , ruleNumbersPrefixWithMinus
  , ruleNumbersSuffixesKMG
  ]
