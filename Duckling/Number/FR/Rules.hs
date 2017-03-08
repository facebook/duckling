-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Number.FR.Rules
  ( rules ) where

import qualified Data.Text as Text
import Prelude
import Data.Maybe
import Data.String

import Duckling.Dimensions.Types
import Duckling.Number.Helpers
import Duckling.Number.Types (NumberData (..))
import qualified Duckling.Number.Types as TNumber
import Duckling.Regex.Types
import Duckling.Types

ruleNumbers4 :: Rule
ruleNumbers4 = Rule
  { name = "numbers 81"
  , pattern =
    [ numberWith TNumber.value (== 80)
    , numberWith TNumber.value (== 1)
    ]
  , prod = \_ -> integer 81
  }

ruleNumbersPrefixWithNegativeOrMinus :: Rule
ruleNumbersPrefixWithNegativeOrMinus = Rule
  { name = "numbers prefix with -, negative or minus"
  , pattern =
    [ regex "-|moins"
    , dimension DNumber
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token DNumber (NumberData {TNumber.value = v}):
       _) -> double $ v * (-1)
      _ -> Nothing
  }

ruleIntegerNumeric :: Rule
ruleIntegerNumeric = Rule
  { name = "integer (numeric)"
  , pattern =
    [ regex "(\\d{1,18})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> do
         v <- parseInt match
         integer $ toInteger v
      _ -> Nothing
  }

ruleNumbers2 :: Rule
ruleNumbers2 = Rule
  { name = "numbers 22..29 32..39 .. 52..59"
  , pattern =
    [ oneOf [20, 50, 40, 30]
    , numberBetween 2 10
    ]
  , prod = \tokens -> case tokens of
      (Token DNumber (NumberData {TNumber.value = v1}):
       Token DNumber (NumberData {TNumber.value = v2}):
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleDecimalWithThousandsSeparator :: Rule
ruleDecimalWithThousandsSeparator = Rule
  { name = "decimal with thousands separator"
  , pattern =
    [ regex "(\\d+(\\.\\d\\d\\d)+,\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> let dot = Text.singleton '.'
                 comma = Text.singleton ','
                 fmt = Text.replace comma dot $ Text.replace dot Text.empty match
        in parseDouble fmt >>= double
      _ -> Nothing
  }

ruleDecimalNumber :: Rule
ruleDecimalNumber = Rule
  { name = "decimal number"
  , pattern =
    [ regex "(\\d*,\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> parseDecimal False match
      _ -> Nothing
  }

ruleNumber2 :: Rule
ruleNumber2 = Rule
  { name = "number (20..60)"
  , pattern =
    [ regex "(vingt|trente|quarante|cinquante|soixante)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch ("vingt":_)):_)     -> integer 20
      (Token RegexMatch (GroupMatch ("trente":_)):_)    -> integer 30
      (Token RegexMatch (GroupMatch ("quarante":_)):_)  -> integer 40
      (Token RegexMatch (GroupMatch ("cinquante":_)):_) -> integer 50
      (Token RegexMatch (GroupMatch ("soixante":_)):_)  -> integer 60
      _                                                 -> Nothing
  }

ruleNumbers5 :: Rule
ruleNumbers5 = Rule
  { name = "numbers 62..69 .. 92..99"
  , pattern =
    [ oneOf [60, 80]
    , numberBetween 2 20
    ]
  , prod = \tokens -> case tokens of
      (Token DNumber (NumberData {TNumber.value = v1}):
       Token DNumber (NumberData {TNumber.value = v2}):
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleNumber :: Rule
ruleNumber = Rule
  { name = "number (0..16)"
  , pattern =
    [ regex "(z(e|\x00e9)ro|une?|deux|trois|quatre|cinq|six|sept|huit|neuf|dix|onze|douze|treize|quatorze|quinze|seize)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch ("zero":_)):_)      -> integer 0
      (Token RegexMatch (GroupMatch ("z\x00e9ro":_)):_) -> integer 0
      (Token RegexMatch (GroupMatch ("un":_)):_)        -> integer 1
      (Token RegexMatch (GroupMatch ("une":_)):_)       -> integer 1
      (Token RegexMatch (GroupMatch ("deux":_)):_)      -> integer 2
      (Token RegexMatch (GroupMatch ("trois":_)):_)     -> integer 3
      (Token RegexMatch (GroupMatch ("quatre":_)):_)    -> integer 4
      (Token RegexMatch (GroupMatch ("cinq":_)):_)      -> integer 5
      (Token RegexMatch (GroupMatch ("six":_)):_)       -> integer 6
      (Token RegexMatch (GroupMatch ("sept":_)):_)      -> integer 7
      (Token RegexMatch (GroupMatch ("huit":_)):_)      -> integer 8
      (Token RegexMatch (GroupMatch ("neuf":_)):_)      -> integer 9
      (Token RegexMatch (GroupMatch ("dix":_)):_)       -> integer 10
      (Token RegexMatch (GroupMatch ("onze":_)):_)      -> integer 11
      (Token RegexMatch (GroupMatch ("douze":_)):_)     -> integer 12
      (Token RegexMatch (GroupMatch ("treize":_)):_)    -> integer 13
      (Token RegexMatch (GroupMatch ("quatorze":_)):_)  -> integer 14
      (Token RegexMatch (GroupMatch ("quinze":_)):_)    -> integer 15
      (Token RegexMatch (GroupMatch ("seize":_)):_)     -> integer 16
      _                                                 -> Nothing
  }

ruleNumber3 :: Rule
ruleNumber3 = Rule
  { name = "number (17..19)"
  , pattern =
    [ numberWith TNumber.value (== 10)
    , numberBetween 7 10
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token DNumber (NumberData {TNumber.value = v}):
       _) -> double $ 10 + v
      _ -> Nothing
  }

ruleNumbers3 :: Rule
ruleNumbers3 = Rule
  { name = "numbers 61 71"
  , pattern =
    [ numberWith TNumber.value (== 60)
    , regex "-?et-?"
    , oneOf [1, 11]
    ]
  , prod = \tokens -> case tokens of
      (Token DNumber (NumberData {TNumber.value = v1}):
       _:
       Token DNumber (NumberData {TNumber.value = v2}):
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleNumbersSuffixesKMG :: Rule
ruleNumbersSuffixesKMG = Rule
  { name = "numbers suffixes (K, M, G)"
  , pattern =
    [ dimension DNumber
    , regex "([kmg])(?=[\\W$\x20ac\x00a2\x00a3]|$)"
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

ruleNumber4 :: Rule
ruleNumber4 = Rule
  { name = "number 80"
  , pattern =
    [ regex "quatre"
    , regex "vingts?"
    ]
  , prod = \_ -> integer 80
  }

ruleNumbers :: Rule
ruleNumbers = Rule
  { name = "numbers 21 31 41 51"
  , pattern =
    [ oneOf [20, 50, 40, 30]
    , regex "et"
    , numberWith TNumber.value (== 1)
    ]
  , prod = \tokens -> case tokens of
      (Token DNumber (NumberData {TNumber.value = v1}):
       _:
       Token DNumber (NumberData {TNumber.value = v2}):
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleIntegerWithThousandsSeparator :: Rule
ruleIntegerWithThousandsSeparator = Rule
  { name = "integer with thousands separator ."
  , pattern =
    [ regex "(\\d{1,3}(\\.\\d\\d\\d){1,5})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> let fmt = Text.replace (Text.singleton '.') Text.empty match
        in parseDouble fmt >>= double
      _ -> Nothing
  }

rulePowersOfTen :: Rule
rulePowersOfTen = Rule
  { name = "powers of tens"
  , pattern =
    [ regex "(cent|mille|millions?|milliards?)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "cent"      -> double 1e2 >>= withGrain 2 >>= withMultipliable
        "mille"     -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "million"   -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "millions"  -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "milliard"  -> double 1e9 >>= withGrain 9 >>= withMultipliable
        "milliards" -> double 1e9 >>= withGrain 9 >>= withMultipliable
        _           -> Nothing
      _ -> Nothing
  }

ruleSum :: Rule
ruleSum = Rule
  { name = "intersect 2 numbers"
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

rules :: [Rule]
rules =
  [ ruleDecimalNumber
  , ruleDecimalWithThousandsSeparator
  , ruleIntegerNumeric
  , ruleIntegerWithThousandsSeparator
  , ruleNumber
  , ruleNumber2
  , ruleNumber3
  , ruleNumber4
  , ruleNumbers
  , ruleNumbers2
  , ruleNumbers3
  , ruleNumbers4
  , ruleNumbers5
  , ruleNumbersPrefixWithNegativeOrMinus
  , ruleNumbersSuffixesKMG
  , rulePowersOfTen
  , ruleSum
  , ruleMultiply
  ]
