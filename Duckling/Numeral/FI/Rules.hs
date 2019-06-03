-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Numeral.FI.Rules
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
import Duckling.Numeral.Types (NumeralData(..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral

ruleIntegerNumeric :: Rule
ruleIntegerNumeric = Rule
  { name = "integer (numeric)"
  , pattern =
    [ regex "(\\d{1,18})"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        v <- parseInt match
        integer $ toInteger v
      _ -> Nothing
  }

numeralMap :: HashMap Text Integer
numeralMap = HashMap.fromList
  [ ( "nolla", 0 )
  , ( "yksi", 1 )
  , ( "kaksi", 2 )
  , ( "kolme", 3 )
  , ( "neljä", 4 )
  , ( "viisi", 5 )
  , ( "kuusi", 6 )
  , ( "seitsemän", 7 )
  , ( "kahdeksan", 8 )
  , ( "yhdeksän", 9 )
  , ( "kymmenen", 10 )
  ]

ruleNumeral :: Rule
ruleNumeral = Rule
  { name = "number (0..10)"
  , pattern =
    [ regex "(nolla|yksi|kaksi|kolme|neljä|viisi|kuusi|seitsemän|kahdeksan|yhdeksän|kymmenen)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) numeralMap >>= integer
      _ -> Nothing
  }

elevenToNineteenMap :: HashMap Text Integer
elevenToNineteenMap = HashMap.fromList
  [ ( "yksitoista", 11 )
  , ( "kaksitoista", 12 )
  , ( "kolmetoista", 13 )
  , ( "neljätoista", 14 )
  , ( "viisitoista", 15 )
  , ( "kuusitoista", 16 )
  , ( "seitsemäntoista", 17 )
  , ( "kahdeksantoista", 18 )
  , ( "yhdeksäntoista", 19 )
  ]

ruleElevenToNineteen :: Rule
ruleElevenToNineteen = Rule
  { name = "number (11..19)"
  , pattern =
    [ regex "(yksitoista|kaksitoista|kolmetoista|neljätoista|viisitoista|kuusitoista|seitsemäntoista|kahdeksantoista|yhdeksäntoista)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) elevenToNineteenMap >>= integer
      _ -> Nothing
  }

tensMap :: HashMap Text Integer
tensMap = HashMap.fromList
  [ ( "kaksikymmentä", 20 )
  , ( "kolmekymmentä", 30 )
  , ( "neljäkymmentä", 40 )
  , ( "viisikymmentä", 50 )
  , ( "kuusikymmentä", 60 )
  , ( "seitsemänkymmentä", 70 )
  , ( "kahdeksankymmentä", 80 )
  , ( "yhdeksänkymmentä", 90 )
  ]

ruleTens :: Rule
ruleTens = Rule
  { name = "integer (20,30..90)"
  , pattern =
    [ regex "(kaksikymmentä|kolmekymmentä|neljäkymmentä|viisikymmentä|kuusikymmentä|seitsemänkymmentä|kahdeksankymmentä|yhdeksänkymmentä)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) tensMap >>= integer
      _ -> Nothing
  }

ruleCompositeTens :: Rule
ruleCompositeTens = Rule
  { name = "integer ([2-9][1-9])"
  , pattern =
    [ regex "(kaksikymmentä|kolmekymmentä|neljäkymmentä|viisikymmentä|kuusikymmentä|seitsemänkymmentä|kahdeksankymmentä|yhdeksänkymmentä)(yksi|kaksi|kolme|neljä|viisi|kuusi|seitsemän|kahdeksan|yhdeksän)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        v1 <- HashMap.lookup (Text.toLower m1) tensMap
        v2 <- HashMap.lookup (Text.toLower m2) numeralMap
        integer $ v1 + v2
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleIntegerNumeric
  , ruleNumeral
  , ruleElevenToNineteen
  , ruleTens
  , ruleCompositeTens
  ]
