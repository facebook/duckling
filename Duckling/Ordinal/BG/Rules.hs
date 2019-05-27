-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Ordinal.BG.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
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
  [ ( "първ"         , 1 )
  , ( "втор"         , 2 )
  , ( "трет"         , 3 )
  , ( "четвърт"      , 4 )
  , ( "пет"          , 5 )
  , ( "шест"         , 6 )
  , ( "седм"         , 7 )
  , ( "осм"          , 8 )
  , ( "девет"        , 9 )
  , ( "десет"        , 10 )
  , ( "единадесет"   , 11 )
  , ( "дванадесет"   , 12 )
  , ( "тринадесет"   , 13 )
  , ( "четиринадесет", 14 )
  , ( "петнадесет"   , 15 )
  , ( "шестнадесет"  , 16 )
  , ( "седемнадесет" , 17 )
  , ( "осемнадесет"  , 18 )
  , ( "деветнадесет" , 19 )
  , ( "двадесет"     , 20 )
  ]

ruleOrdinalsFirstth :: Rule
ruleOrdinalsFirstth = Rule
  { name = "ordinals (first..19th)"
  , pattern =
    [ regex "(първ|втор|трет|четвърт|пет|шест|седм|осм|девет|десет|единадесет|дванадесет|тринадесет|четиринадесет|петнадесет|шестнадесет|седемнадесет|осемнадесет|деветнадесет|двадесет)(и(я(т)?|те)?|а(та)?|о(то)?)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> HashMap.lookup (Text.toLower match) ordinalsMap
      _ -> Nothing
  }

dozensMap :: HashMap Text Int
dozensMap = HashMap.fromList
  [ ( "двадесет"  , 20 )
  , ( "тридесет"  , 30 )
  , ( "четирдесет", 40 )
  , ( "петдесет"  , 50 )
  , ( "шестдесет" , 60 )
  , ( "седемдесет", 70 )
  , ( "осемдесет" , 80 )
  , ( "деветдесет", 90 )
  ]

ruleOrdinal :: Rule
ruleOrdinal = Rule
  { name = "ordinal 21..99"
  , pattern =
    [ regex "(двадесет|тридесет|четирдесет|петдесет|шестдесет|седемдесет|осемдесет|деветдесет)"
    , regex "и (първ|втор|трет|четвърт|пет|шест|седм|осм|девет)(и(ят?|те)?|а(та)?|о(то)?)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:_)):
       Token RegexMatch (GroupMatch (m2:_)):
       _) -> do
         dozen <- HashMap.lookup (Text.toLower m1) dozensMap
         unit <- HashMap.lookup (Text.toLower m2) ordinalsMap
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
