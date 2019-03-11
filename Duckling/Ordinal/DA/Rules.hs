-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Duckling.Ordinal.DA.Rules
  ( rules ) where

import qualified Data.Text as Text
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Numeral.Types (NumeralData (..), getIntValue)
import qualified Duckling.Numeral.Types as TNumeral
import Duckling.Ordinal.Helpers
import Duckling.Ordinal.Types (OrdinalData (..))
import Duckling.Regex.Types
import Duckling.Types

ruleOrdinalsFirstst :: Rule
ruleOrdinalsFirstst = Rule
  { name = "ordinals (first..19st)"
  , pattern =
    [ regex "(første|anden|tredje|fjerde|femte|sjette|syvende|ottende|niende|tiende|elfte|tolvte|trettende|fjortende|femtende|sekstende|syttende|attende|nittende)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "første" -> Just $ ordinal 1
        "anden" -> Just $ ordinal 2
        "tredje" -> Just $ ordinal 3
        "fjerde" -> Just $ ordinal 4
        "femte" -> Just $ ordinal 5
        "sjette" -> Just $ ordinal 6
        "syvende" -> Just $ ordinal 7
        "ottende" -> Just $ ordinal 8
        "niende" -> Just $ ordinal 9
        "tiende" -> Just $ ordinal 10
        "elfte" -> Just $ ordinal 11
        "tolvte" -> Just $ ordinal 12
        "trettende" -> Just $ ordinal 13
        "fjortende" -> Just $ ordinal 14
        "femtende" -> Just $ ordinal 15
        "sekstende" -> Just $ ordinal 16
        "syttende" -> Just $ ordinal 17
        "attende" -> Just $ ordinal 18
        "nittende" -> Just $ ordinal 19
        "tyvende" -> Just $ ordinal 20
        "tenogtyvende" -> Just $ ordinal 21
        "toogtyvende" -> Just $ ordinal 22
        "treogtyvende" -> Just $ ordinal 23
        "fireogtyvende" -> Just $ ordinal 24
        "femogtyvende" -> Just $ ordinal 25
        "seksogtyvende" -> Just $ ordinal 26
        "syvogtyvende" -> Just $ ordinal 27
        "otteogtyvende" -> Just $ ordinal 28
        "niogtyvende" -> Just $ ordinal 29
        "tredivte" -> Just $ ordinal 30
        "enogtredivte" -> Just $ ordinal 31
        _ -> Nothing
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
      oneVal <- case Text.toLower ones of
                  "" -> Just 0
                  "enog" -> Just 1
                  "toog" -> Just 2
                  "treog" -> Just 3
                  "fireog" -> Just 4
                  "femog" -> Just 5
                  "seksog" -> Just 6
                  "syvog" -> Just 7
                  "otteog" -> Just 8
                  "niog" -> Just 9
                  _ -> Nothing
      tenVal <- case Text.toLower tens of
                  "tyvende" -> Just 20
                  "tredivte" -> Just 30
                  "fyrrende" -> Just 40
                  "fyrretyvende" -> Just 40
                  "halvtredsende" -> Just 50
                  "halvtredsindstyvende" -> Just 50
                  "tressende" -> Just 60
                  "tresindstyvende" -> Just 60
                  "halvfjerdsende" -> Just 70
                  "halvfjerdsindstyvende" -> Just 70
                  "firsende" -> Just 80
                  "firsindsstyvende" -> Just 80
                  "halvfemsende" -> Just 90
                  "halvfemsindstyvende" -> Just 90
                  _ -> Nothing
      Just $ ordinal (oneVal + tenVal) 
    _ -> Nothing
        
  }

ruleSpelledOutBigOrdinals :: Rule
ruleSpelledOutBigOrdinals = Rule
  { name = "ordinals, above 99, spelled out"
  , pattern = 
    [ Predicate (\case
                  Token Numeral NumeralData {TNumeral.value=numnum} | (numnum > 99) -> True
                  _ -> False)
    , regex " og "
    , Predicate (\case
                  Token Ordinal (OrdinalData ordnum) -> True
                  _ -> False)  
    ]
  , prod = \case
      (Token Numeral NumeralData {TNumeral.value=maybenumnum}):_:(Token Ordinal (OrdinalData ordnum)):_ ->
            case getIntValue(maybenumnum) of
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
