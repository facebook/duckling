-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.DE.Rules
  ( rules
  ) where

import Data.Semigroup ((<>))
import Prelude
import Data.String
import Data.Text (Text)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Helpers
import Duckling.Regex.Types
import Duckling.Types

ordinalList :: [(Text, Int)]
ordinalList =
  [ ("erste", 1)
  , ("zweite", 2)
  , ("dritte", 3)
  , ("vierte", 4)
  , ("fünfte", 5)
  , ("sechste", 6)
  , ("siebte", 7)
  , ("achte", 8)
  , ("neunte", 9)
  , ("zehnte", 10)
  , ("elfte", 11)
  , ("zwölfte", 12)
  , ("dreizente", 13)
  , ("vierzehnte", 14)
  , ("fünfzehnte", 15)
  , ("sechzente", 16)
  , ("siebzehnte", 17)
  , ("achtzehnte", 18)
  , ("neunzehnte", 19)
  , ("zwanzigste", 20)
  , ("einundzwanzigste", 21)
  , ("zweiundzwanzigste", 22)
  , ("dreiundzwanzigste", 23)
  , ("vierundzwanzigste", 24)
  , ("fünfundzwanzigste", 25)
  , ("sechsundzwanzigste", 26)
  , ("siebenundzwanzigste", 27)
  , ("achtundzwanzigste", 28)
  , ("neunundzwanzigste", 29)
  , ("dreissigste", 30)
  , ("dreißigste", 30)
  , ("einunddreissigste", 31)
  , ("einunddreißigste", 31)
  ]

ruleOrdinalsFirstth :: Rule
ruleOrdinalsFirstth = Rule
  { name = "ordinal (1..31)"
  , pattern =
    [ regex $ Text.unpack construction
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> HashMap.lookup (Text.toLower match) ordinalMap
      _ -> Nothing
  }
  where
    ordinalMap :: HashMap.HashMap Text Int
    ordinalMap = HashMap.fromList ordinalList

    construction :: Text
    construction =
      "("
      <> mconcat (List.intersperse "|" (fst <$> ordinalList))
      <> ")[rsn]?"

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern =
    [ regex "(?<!\\d|\\.)0*(\\d+)(\\.(?!\\d)| ?(te(n|r|s)?)|(ste(n|r|s)?))"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        v <- parseInt match
        Just $ ordinal v
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleOrdinalsFirstth
  , ruleOrdinalDigits
  ]
