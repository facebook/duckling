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
    , regex "\x1006\x101a\x103a\x1037"
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
    [ regex "(\x1040|\x1041|\x1042|\x1043|\x1044|\x1045|\x1046|\x1047|\x1048|\x1049)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case match of
        "\x1040" -> integer 0
        "\x1041" -> integer 1
        "\x1042" -> integer 2
        "\x1043" -> integer 3
        "\x1044" -> integer 4
        "\x1045" -> integer 5
        "\x1046" -> integer 6
        "\x1047" -> integer 7
        "\x1048" -> integer 8
        "\x1049" -> integer 9
        _ -> Nothing
      _ -> Nothing
  }

ruleInteger3 :: Rule
ruleInteger3 = Rule
  { name = "integer (11..19) "
  , pattern =
    [ regex "\x1006\x101a\x103a\x1037"
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
    [ regex "(\x1015\x1011\x1019|\x1012\x102f\x1010\x102d\x101a|\x1010\x1010\x102d\x101a)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case match of
        "\x1015\x1011\x1019" -> integer 1
        "\x1012\x102f\x1010\x102d\x101a" -> integer 2
        "\x1010\x1010\x102d\x101a" -> integer 3
        _ -> Nothing
      _ -> Nothing
  }

ruleInteger6 :: Rule
ruleInteger6 = Rule
  { name = "integer (100..900)"
  , pattern =
    [ numberBetween 1 10
    , regex "\x101b\x102c"
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
    , regex "\x1011\x1031\x102c\x1004\x103a"
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
    , regex "\x101e\x1031\x102c\x1004\x103a\x1038"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v}):_) -> double $ v * 10000
      _ -> Nothing
  }

ruleInteger :: Rule
ruleInteger = Rule
  { name = "integer 0"
  , pattern =
    [ regex "\x101e\x102f\x1036\x100a|\x1019\x101b\x103e\x102d"
    ]
  , prod = \_ -> integer 0
  }

ruleInteger4 :: Rule
ruleInteger4 = Rule
  { name = "integer (10..90)"
  , pattern =
    [ numberBetween 1 10
    , regex "\x1006\x101a\x103a"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v}):_) -> double $ v * 10
      _ -> Nothing
  }

ruleInteger2 :: Rule
ruleInteger2 = Rule
  { name = "integer (1..10)"
  , pattern =
    [ regex "(\x1010\x1005\x103a|\x1014\x103e\x1005\x103a|\x101e\x102f\x1036\x1038|\x101c\x1031\x1038|\x1004\x102b\x1038|\x1001\x103c\x1031\x102b\x1000\x103a|\x1001\x102f\x1014\x103e\x1005\x103a|\x101b\x103e\x1005\x103a|\x1000\x102d\x102f\x1038|\x1010\x1005\x103a\x1006\x101a\x103a)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case match of
        "\x1010\x1005\x103a" -> integer 1
        "\x1014\x103e\x1005\x103a" -> integer 2
        "\x101e\x102f\x1036\x1038" -> integer 3
        "\x101c\x1031\x1038" -> integer 4
        "\x1004\x102b\x1038" -> integer 5
        "\x1001\x103c\x1031\x102b\x1000\x103a" -> integer 6
        "\x1001\x102f\x1014\x103e\x1005\x103a" -> integer 7
        "\x101b\x103e\x1005\x103a" -> integer 8
        "\x1000\x102d\x102f\x1038" -> integer 9
        "\x1010\x1005\x103a\x1006\x101a\x103a" -> integer 10
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
