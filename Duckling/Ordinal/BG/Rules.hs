-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.BG.Rules
  ( rules ) where

import Control.Monad (join)
import qualified Data.Text as Text
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Helpers
import Duckling.Regex.Types
import Duckling.Types

ruleOrdinalsFirstth :: Rule
ruleOrdinalsFirstth = Rule
  { name = "ordinals (first..19th)"
  , pattern =
    [ regex "(първ|втор|трет|четвърт|пет|шест|седм|осм|девет|десет|единадесет|дванадесет|тринадесет|четиринадесет|петнадесет|шестнадесет|седемнадесет|осемнадесет|деветнадесет|двадесет)(и(я(т)?|те)?|а(та)?|о(то)?)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "първ" -> Just $ ordinal 1
        "втор" -> Just $ ordinal 2
        "трет" -> Just $ ordinal 3
        "четвърт" -> Just $ ordinal 4
        "пет" -> Just $ ordinal 5
        "шест" -> Just $ ordinal 6
        "седм" -> Just $ ordinal 7
        "осм" -> Just $ ordinal 8
        "девет" -> Just $ ordinal 9
        "десет" -> Just $ ordinal 10
        "единадесет" -> Just $ ordinal 11
        "дванадесет" -> Just $ ordinal 12
        "тринадесет" -> Just $ ordinal 13
        "четиринадесет" -> Just $ ordinal 14
        "петнадесет" -> Just $ ordinal 15
        "шестнадесет" -> Just $ ordinal 16
        "седемнадесет" -> Just $ ordinal 17
        "осемнадесет" -> Just $ ordinal 18
        "деветнадесет" -> Just $ ordinal 19
        "двадесет" -> Just $ ordinal 20
        _ -> Nothing
      _ -> Nothing
  }

ruleOrdinal :: Rule
ruleOrdinal = Rule
  { name = "ordinal 21..99"
  , pattern =
    [ regex "(двадесет|тридесет|четирдесет|петдесет|шестдесет|седемдесет|осемдесет|деветдесет)"
    , regex "и (първ|втор|трет|четвърт|пет|шест|седм|осм|девет)(и(я(т)?|те)?|а(та)?|о(то)?)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:_)):
       Token RegexMatch (GroupMatch (m2:_)):
       _) -> do
         dozen <- case Text.toLower m1 of
           "двадесет" -> Just 20
           "тридесет" -> Just 30
           "четирдесет" -> Just 40
           "петдесет" -> Just 50
           "шестдесет" -> Just 60
           "седемдесет" -> Just 70
           "осемдесет" -> Just 80
           "деветдесет" -> Just 90
           _ -> Nothing
         unit <- case Text.toLower m2 of
           "първ" -> Just 1
           "втор" -> Just 2
           "трет" -> Just 3
           "четвърт" -> Just 4
           "пет" -> Just 5
           "шест" -> Just 6
           "седм" -> Just 7
           "осм" -> Just 8
           "девет" -> Just 9
           _ -> Nothing
         Just . ordinal $ dozen + unit
      _ -> Nothing
  }

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern =
    [ regex "0*(\\d+)-?((в|р|м|т)(и(я(т)?|те)?|а(та)?|о(то)?))"
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
