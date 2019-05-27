-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Ordinal.DA.Rules
  ( rules ) where


import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt, numberWith)
import Duckling.Numeral.Types (NumeralData (..), getIntValue)
import Duckling.Ordinal.Helpers
import Duckling.Ordinal.Types (OrdinalData (..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral

ordinalsMap :: HashMap Text Int
ordinalsMap = HashMap.fromList
  [ ( "første", 1 )
  , ( "anden", 2 )
  , ( "tredje", 3 )
  , ( "fjerde", 4 )
  , ( "femte", 5 )
  , ( "sjette", 6 )
  , ( "syvende", 7 )
  , ( "ottende", 8 )
  , ( "niende", 9 )
  , ( "tiende", 10 )
  , ( "elfte", 11 )
  , ( "tolvte", 12 )
  , ( "trettende", 13 )
  , ( "fjortende", 14 )
  , ( "femtende", 15 )
  , ( "sekstende", 16 )
  , ( "syttende", 17 )
  , ( "attende", 18 )
  , ( "nittende", 19 )
  , ( "tyvende", 20 )
  , ( "tenogtyvende", 21 )
  , ( "toogtyvende", 22 )
  , ( "treogtyvende", 23 )
  , ( "fireogtyvende", 24 )
  , ( "femogtyvende", 25 )
  , ( "seksogtyvende", 26 )
  , ( "syvogtyvende", 27 )
  , ( "otteogtyvende", 28 )
  , ( "niogtyvende", 29 )
  , ( "tredivte", 30 )
  , ( "enogtredivte", 31 )
  ]

cardinalsMap :: HashMap Text Int
cardinalsMap = HashMap.fromList
  [ ( "tyvende", 20 )
  , ( "tredivte", 30 )
  , ( "fyrrende", 40 )
  , ( "fyrretyvende", 40 )
  , ( "halvtredsende", 50 )
  , ( "halvtredsindstyvende", 50 )
  , ( "tressende", 60 )
  , ( "tresindstyvende", 60 )
  , ( "halvfjerdsende", 70 )
  , ( "halvfjerdsindstyvende", 70 )
  , ( "firsende", 80 )
  , ( "firsindsstyvende", 80 )
  , ( "halvfemsende", 90 )
  , ( "halvfemsindstyvende", 90 )
  ]

oneValMap :: HashMap Text Int
oneValMap = HashMap.fromList
  [ ( "", 0 )
  , ( "enog", 1 )
  , ( "toog", 2 )
  , ( "treog", 3 )
  , ( "fireog", 4 )
  , ( "femog", 5 )
  , ( "seksog", 6 )
  , ( "syvog", 7 )
  , ( "otteog", 8 )
  , ( "niog", 9 )
  ]

ruleOrdinalsFirstst :: Rule
ruleOrdinalsFirstst = Rule
  { name = "ordinals (first..19st)"
  , pattern =
    [ regex "(første|anden|tredje|fjerde|femte|sjette|syvende|ottende|niende|tiende|elfte|tolvte|trettende|fjortende|femtende|sekstende|syttende|attende|nittende)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> HashMap.lookup (Text.toLower match) ordinalsMap
      _ -> Nothing
  }

ruleSpelledOutOrdinals :: Rule
ruleSpelledOutOrdinals = Rule
  { name = "ordinals, 20 to 99, spelled-out"
  , pattern =
    [ regex (concat ["((?:en|to|tre|fire|fem|seks|syv|otte|ni)og)?",
                     "(tyvende",
                     "|tredivte",
                     "|fyrr(?:etyv)?ende",
                     "|halvtreds(?:indstyv)?ende",
                     "|tres(?:indstyv|s)?ende",
                     "|halvfjerds(?:indstyv)?ende",
                     "|firs(?:indstyv)?ende",
                     "|halvfems(?:indstyv)?ende)"])
    ]
  , prod = \case
    (Token RegexMatch (GroupMatch (ones:tens:_)):_) -> do
      oneVal <- HashMap.lookup (Text.toLower ones) oneValMap
      tenVal <- HashMap.lookup (Text.toLower tens) cardinalsMap
      Just $ ordinal (oneVal + tenVal)
    _ -> Nothing

  }

ruleSpelledOutBigOrdinals :: Rule
ruleSpelledOutBigOrdinals = Rule
  { name = "ordinals, above 99, spelled out"
  , pattern =
    [ numberWith TNumeral.value (> 99)
    , regex "og"
    , dimension Ordinal
    ]
  , prod = \case
      Token Numeral NumeralData {TNumeral.value=maybenumnum}:_:Token Ordinal (OrdinalData ordnum):_ ->
            case getIntValue maybenumnum of
              Just numnum -> Just $ ordinal (numnum + ordnum)
              Nothing -> Nothing
      _ -> Nothing
  }

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern =
    [ regex "0*(\\d+)(\\.|ste?)"
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
  , ruleOrdinalsFirstst
  , ruleSpelledOutOrdinals
  , ruleSpelledOutBigOrdinals
  ]
