-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Ordinal.MN.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Helpers
import Duckling.Regex.Types
import Duckling.Types

ordinalsFirstthMap :: HashMap Text.Text Int
ordinalsFirstthMap = HashMap.fromList
  [ ( "нэг", 1 )
  , ( "хоёр", 2 )
  , ( "гурав", 3 )
  , ( "дөрөв", 4 )
  , ( "тав", 5 )
  , ( "зургаа", 6 )
  , ( "долоо", 7 )
  , ( "найм", 8 )
  , ( "ес", 9 )
  , ( "арав", 10 )
   ]

cardinalsMap :: HashMap Text.Text Int
cardinalsMap = HashMap.fromList
  [ ( "арван", 10 )
  , ( "хорин", 20 )
  , ( "хорь", 20 )
  , ( "гучин", 30 )
  , ( "гуч", 30 )
  , ( "дөчин", 40 )
  , ( "дөч", 40 )
  , ( "тавин", 50 )
  , ( "тавь", 50 )
  , ( "жаран", 60 )
  , ( "жар", 60 )
  , ( "далан", 70 )
  , ( "дал", 70 )
  , ( "наян", 80 )
  , ( "ная", 80 )
  , ( "ерэн", 90 )
  , ( "ер", 90 )
  ]



ruleOrdinalsFirstth :: Rule
ruleOrdinalsFirstth = Rule
  { name = "ordinals (first..19th)"
  , pattern =
    [ regex "(нэг|хоёр|гурав|дөрөв|тав|зургаа|долоо|найм|ес|арав) ?(дугаар|дүгээр|дахь|дэх)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> HashMap.lookup (Text.toLower match) ordinalsFirstthMap
      _ -> Nothing
  }

ruleOrdinal :: Rule
ruleOrdinal = Rule
  { name = "ordinal 10..99"
  , pattern =
    [ regex "(арван|хорин|гучин|дөчин|тавин|жаран|далан|наян|ерэн) ?(нэг|хоёр|гурав|дөрөв|тав|зургаа|долоо|найм|ес) ?(дугаар|дүгээр|дахь|дэх)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:_)):
       Token RegexMatch (GroupMatch (m2:_)):
       _) -> do
         dozen <- HashMap.lookup (Text.toLower m1) cardinalsMap
         unit <- HashMap.lookup (Text.toLower m2) ordinalsFirstthMap
         Just . ordinal $ dozen + unit
      _ -> Nothing
  }

 -- TODO: Single-word composition (#110)
ruleInteger2 :: Rule
ruleInteger2 = Rule
  { name = "integer (20..90)"
  , pattern =
    [ regex "(хорь|гуч|дөч|тавь|жар|дал|ная|ер) ?(дугаар|дүгээр|дахь|дэх)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
         ordinal <$> HashMap.lookup (Text.toLower match) cardinalsMap
      _ -> Nothing
  }

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern =
    [ regex "0*(\\d+)-?(ын|ийн|р|с|)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> ordinal <$> parseInt match
      _ -> Nothing
  }

ruleOrdinalDigits2 :: Rule
ruleOrdinalDigits2 = Rule
  { name = "ordinal (digits)"
  , pattern =
    [ regex "(?<!\\d|\\.)0*(\\d+)(\\.(?!\\d)| ?(дугаар|дүгээр|дахь|дэх))"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        v <- parseInt match
        Just $ ordinal v
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleOrdinalDigits
  , ruleOrdinalDigits2
  , ruleOrdinal
  , ruleInteger2
  , ruleOrdinalsFirstth
  ]
