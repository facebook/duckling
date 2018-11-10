-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


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
  , ( "арваннэг", 11 )
  , ( "арванхоёр", 12 )
  , ( "арвангурав", 13 )
  , ( "арвандөрөв", 14 )
  , ( "арвантав", 15 )
  , ( "арванзургаа", 16 )
  , ( "арвандолоо", 17 )
  , ( "арваннайм", 18 )
  , ( "арванес", 19 )
  , ( "хорь", 20 )
  ]

cardinalsMap :: HashMap Text.Text Int
cardinalsMap = HashMap.fromList
  [ ( "хорь", 20 )
  , ( "гуч", 30 )
  , ( "дөч", 40 )
  , ( "тавь", 50 )
  , ( "жар", 60 )
  , ( "дал", 70 )
  , ( "ная", 80 )
  , ( "ер", 90 )
  ]

ruleOrdinalsFirstth :: Rule
ruleOrdinalsFirstth = Rule
  { name = "ordinals (first..19th)"
  , pattern =
    [ regex "(нэг|хоёр|гурав|дөрөв|тав|зургаа|долоо|найм|ес|арав|арван нэг|арван хоёр|арван гурав|арван дөрөв|арван тав|арван зургаа|арван долоо|арван найм|арван ес|хорь)(дугаар|дүгээр| дахь| дэх)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> HashMap.lookup (Text.toLower match) ordinalsFirstthMap
      _ -> Nothing
  }

ruleOrdinal :: Rule
ruleOrdinal = Rule
  { name = "ordinal 21..99"
  , pattern =
    [ regex "(хорин|гучин|дөчин|тавин|жаран|далан|наян|ерэн)"
    , regex "(нэг|хоёр|гурав|дөрөв|тав|зургаа|долоо|найм|ес)(дугаар|дүгээр| дахь)"
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

rules :: [Rule]
rules =
  [ ruleOrdinal
  , ruleOrdinalDigits
  , ruleOrdinalsFirstth
  ]
