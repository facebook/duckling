-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.UK.Rules
  ( rules ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Control.Monad (join)
import Data.Text (Text)
import qualified Data.Text as Text
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Helpers
import Duckling.Regex.Types
import Duckling.Types

ordinalsFirstThMap :: HashMap Text Int
ordinalsFirstThMap = HashMap.fromList
  [ ( "перш"                   , 1 )
  , ( "друг"                   , 2 )
  , ( "трет"                   , 3 )
  , ( "четверт" , 4 )
  , ( "п‘ят"                   , 5 )
  , ( "шост"                   , 6 )
  , ( "сьом"                   , 7 )
  , ( "восьм"             , 8 )
  , ( "дев‘ят"       , 9 )
  , ( "десят"             , 10 )
  , ( "одинадцят"             , 11 )
  , ( "дванадцят"             , 12 )
  , ( "тринадцят"             , 13 )
  , ( "чотирнадцят" , 14 )
  , ( "п‘ятнадцят"       , 15 )
  , ( "шістнадцят"       , 16 )
  , ( "сімнадцят"             , 17 )
  , ( "вісімнадцят" , 18 )
  , ( "дев‘ятнадцят" , 19 )
  , ( "двадцят"                               , 20 )
  ]

ruleOrdinalsFirstth :: Rule
ruleOrdinalsFirstth = Rule
  { name = "ordinals (first..19th)"
  , pattern =
    [ regex "(перш|друг|трет|четверт|п‘ят|шост|сьом|восьм|дев‘ят|десят|одинадцят|дванадцят|тринадцят|чотирнадцят|п‘ятнадцят|шістнадцят|сімнадцят|вісімнадцят|дев‘ятнадцят|двадцят)(ий|ій|а|я|е|є)"
    ]
  , prod = \tokens -> case tokens of
    (Token RegexMatch (GroupMatch (match:_)):_) ->
      ordinal <$> HashMap.lookup (Text.toLower match) ordinalsFirstThMap
    _ -> Nothing
  }

ordinalTensMap :: HashMap Text Int
ordinalTensMap = HashMap.fromList
  [ ( "двадцять"             , 20 )
  , ( "тридцять"             , 30 )
  , ( "сорок"                               , 40 )
  , ( "п‘ятдесят"       , 50 )
  , ( "шістдесят"       , 60 )
  , ( "сімдесят"             , 70 )
  , ( "вісімдесят" , 80 )
  , ( "дев‘яносто" , 90 )
  ]

ruleOrdinal :: Rule
ruleOrdinal = Rule
  { name = "ordinal 21..99"
  , pattern =
    [ regex "(двадцять|тридцять|сорок|п‘ятдесят|шістьдесят|сімдесят|вісімдесят|дев‘яносто)"
    , regex "(перш|друг|трет|четверт|п‘ят|шост|сьом|восьм|дев‘ят)(ий|ій|а|я|е|є)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:_)):
       Token RegexMatch (GroupMatch (m2:_)):
       _) -> do
        v1 <- HashMap.lookup (Text.toLower m1) ordinalTensMap
        v2 <- HashMap.lookup (Text.toLower m2) ordinalsFirstThMap -- map to 1..9
        Just . ordinal $ v1 + v2
      _ -> Nothing
  }

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern =
    [ regex "0*(\\d+)-?((и|і)?й|а|я|е|є)"
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
