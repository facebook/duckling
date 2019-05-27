-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.ET.Rules
  ( rules ) where

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

ordinalsMap :: HashMap Text Int
ordinalsMap = HashMap.fromList
 [ ("esimene", 1)
 , ("teine", 2)
 , ("kolmas", 3)
 , ("neljas", 4)
 , ("viies", 5)
 , ("kuues", 6)
 , ("seitsmes", 7)
 , ("kaheksas", 8)
 , ("üheksas", 9)
 , ("kümnes", 10)
 , ("üheteistkümnes", 11)
 , ("kaheteistkümnes", 12)
 , ("kolmeteistkümnes", 13)
 , ("neljateistkümnes", 14)
 , ("viieteistkümnes", 15)
 , ("kuueteistkümnes", 16)
 , ("seitsmeteistkümnes", 17)
 , ("kaheksateistkümnes", 18)
 , ("üheksateistkümnes", 19)
 ]

ruleOrdinalsFirstth :: Rule
ruleOrdinalsFirstth = Rule
  { name = "ordinals (first..19th)"
  , pattern =
    [ regex "(esimene|teine|kolmas|neljas|viies|kuues|seitsmes|kaheksas|üheksas|kümnes|üheteistkümnes|kaheteistkümnes|kolmeteistkümnes|neljateistkümnes|viieteistkümnes|kuueteistkümnes|seitsmeteistkümnes|kaheksateistkümnes|üheksateistkümnes)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> HashMap.lookup (Text.toLower match) ordinalsMap
      _ -> Nothing
  }

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern =
    [ regex "0*(\\d+)\\."
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> ordinal <$> parseInt match
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleOrdinalDigits
  , ruleOrdinalsFirstth
  ]
