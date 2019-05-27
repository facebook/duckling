-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Numeral.LO.Rules
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
  [ ( "ສູນ", 0 )
  , ( "ໜຶ່ງ", 1 )
  , ( "ສອງ", 2 )
  , ( "ສາມ", 3 )
  , ( "ສີ່", 4 )
  , ( "ຫ້າ", 5)
  , ( "ຫົກ", 6)
  , ( "ເຈັດ", 7)
  , ( "ແປດ", 8)
  , ( "ເກົ້າ", 9)
  , ( "ສິບ", 10)
  ]

ruleNumeral :: Rule
ruleNumeral = Rule
  { name = "number (0..10)"
  , pattern =
    [ regex "(ສູນ|ໜຶ່ງ|ສອງ|ສາມ|ສີ່|ຫ້າ|ຫົກ|ເຈັດ|ແປດ|ເກົ້າ|ສິບ)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) ruleNumeralMap >>= integer
      _ -> Nothing
  }

elevenToNineteenMap :: HashMap Text Integer
elevenToNineteenMap = HashMap.fromList
  [ ( "ສິບເອັດ", 11 )
  , ( "ສິບສອງ", 12 )
  , ( "ສິບສາມ", 13 )
  , ( "ສິບສີ່", 14 )
  , ( "ສິບຫ້າ", 15 )
  , ( "ສິບຫົກ", 16 )
  , ( "ສິບເຈັດ", 17 )
  , ( "ສິບແປດ", 18 )
  , ( "ສິບເກົ້າ", 19 )
  ]

ruleElevenToNineteen :: Rule
ruleElevenToNineteen = Rule
  { name = "number (11..19)"
  , pattern =
    [ regex "(ສິບເອັດ|ສິບສອງ|ສິບສາມ|ສິບສີ່|ສິບຫ້າ|ສິບຫົກ|ສິບເຈັດ|ສິບແປດ|ສິບເກົ້າ)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) elevenToNineteenMap >>= integer
      _ -> Nothing
  }

twentyoneToTwentynineMap :: HashMap Text Integer
twentyoneToTwentynineMap = HashMap.fromList
  [ ( "ຊາວເອັດ", 21 )
  , ( "ຊາວສອງ", 22 )
  , ( "ຊາວສາມ", 23 )
  , ( "ຊາວສີ່", 24 )
  , ( "ຊາວຫ້າ", 25 )
  , ( "ຊາວຫົກ", 26 )
  , ( "ຊາວເຈັດ", 27 )
  , ( "ຊາວແປດ", 28 )
  , ( "ຊາວເກົ້າ", 29 )
  ]

ruleTwentyoneToTwentynine :: Rule
ruleTwentyoneToTwentynine = Rule
  { name = "number (21..29)"
  , pattern =
    [ regex "(ຊາວເອັດ|ຊາວສອງ|ຊາວສາມ|ຊາວສີ່|ຊາວຫ້າ|ຊາວຫົກ|ຊາວເຈັດ|ຊາວແປດ|ຊາວເກົ້າ)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) twentyoneToTwentynineMap >>= integer
      _ -> Nothing
  }

tensMap :: HashMap Text Integer
tensMap = HashMap.fromList
  [ ( "ຊາວ", 20 )
  , ( "ສາມສິບ", 30 )
  , ( "ສິບສີ່", 40 )
  , ( "ຫ້າສິບ", 50 )
  , ( "ຫົກສິບ", 60 )
  , ( "ເຈັດສິບ", 70 )
  , ( "ແປດສິບ", 80 )
  , ( "ເກົ້າສິບ", 90 )
  ]

ruleTens :: Rule
ruleTens = Rule
  { name = "integer (20,30..90)"
  , pattern =
    [ regex "(ຊາວ|ສາມສິບ|ສິບສີ່|ຫ້າສິບ|ຫົກສິບ|ເຈັດສິບ|ແປດສິບ|ເກົ້າສິບ)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) tensMap >>= integer
      _ -> Nothing
  }

ruleCompositeTens :: Rule
ruleCompositeTens = Rule
  { name = "integer ([3-9][1-9])"
  , pattern =
    [ regex "(ສາມສິບ|ສິບສີ່|ຫ້າສິບ|ຫົກສິບ|ເຈັດສິບ|ແປດສິບ|ເກົ້າສິບ)(ໜຶ່ງ|ສອງ|ສາມ|ສີ່|ຫ້າ|ຫົກ|ເຈັດ|ແປດ|ເກົ້າ)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        v1 <- HashMap.lookup (Text.toLower m1) tensMap
        v2 <- HashMap.lookup (Text.toLower m2) ruleNumeralMap
        integer $ v1 + v2
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleNumeral
  , ruleElevenToNineteen
  , ruleTwentyoneToTwentynine
  , ruleTens
  , ruleCompositeTens
  ]
