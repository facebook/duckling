-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.GA.Rules
  ( rules ) where

import qualified Data.Text as Text
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Helpers
import Duckling.Regex.Types
import Duckling.Types

ruleOrdinalsChadDaraEtc :: Rule
ruleOrdinalsChadDaraEtc = Rule
  { name = "ordinals (chéad, dara, etc.)"
  , pattern =
    [ regex "(ch(\x00e9|e)ad|aon(\x00fa|u)|t-aon(\x00fa|u)|dara|tr(\x00ed|i)(\x00fa|u)|ceathr(\x00fa|u)|c(\x00fa|u)igi(\x00fa|u)|s(\x00e9|e)(\x00fa|u)|seacht(\x00fa|u)|ocht(\x00fa|u)|t-ocht(\x00fa|u)|nao(\x00fa|u)|deichi(\x00fa|u)|fichi(\x00fa|u)|tr(\x00ed|i)ochad(\x00fa|u)|daichead(\x00fa|u)|caogad(\x00fa|u)|seascad(\x00fa|u)|seacht(\x00f3|o)d(\x00fa|u)|ocht(\x00f3|o)d(\x00fa|u)|t-ocht(\x00f3|o)d(\x00fa|u)|n(\x00f3|o)chad(\x00fa|u)|c(\x00e9|e)ad(\x00fa|u)|mili(\x00fa|u)|milli(\x00fa|u)n(\x00fa|u))"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "t-aonu" -> Just $ ordinal 1
        "aonu" -> Just $ ordinal 1
        "aon\x00fa" -> Just $ ordinal 1
        "ch\x00e9ad" -> Just $ ordinal 1
        "chead" -> Just $ ordinal 1
        "t-aon\x00fa" -> Just $ ordinal 1
        "dara" -> Just $ ordinal 2
        "tri\x00fa" -> Just $ ordinal 3
        "tr\x00edu" -> Just $ ordinal 3
        "tr\x00ed\x00fa" -> Just $ ordinal 3
        "triu" -> Just $ ordinal 3
        "ceathr\x00fa" -> Just $ ordinal 4
        "ceathru" -> Just $ ordinal 4
        "c\x00faigiu" -> Just $ ordinal 5
        "c\x00faigi\x00fa" -> Just $ ordinal 5
        "cuigiu" -> Just $ ordinal 5
        "cuigi\x00fa" -> Just $ ordinal 5
        "s\x00e9u" -> Just $ ordinal 6
        "s\x00e9\x00fa" -> Just $ ordinal 6
        "seu" -> Just $ ordinal 6
        "se\x00fa" -> Just $ ordinal 6
        "seachtu" -> Just $ ordinal 7
        "seacht\x00fa" -> Just $ ordinal 7
        "t-ocht\x00fa" -> Just $ ordinal 8
        "ochtu" -> Just $ ordinal 8
        "t-ochtu" -> Just $ ordinal 8
        "ocht\x00fa" -> Just $ ordinal 8
        "naou" -> Just $ ordinal 9
        "nao\x00fa" -> Just $ ordinal 9
        "deichiu" -> Just $ ordinal 10
        "deichi\x00fa" -> Just $ ordinal 10
        "fichiu" -> Just $ ordinal 20
        "fichi\x00fa" -> Just $ ordinal 20
        "tr\x00edochadu" -> Just $ ordinal 30
        "triochadu" -> Just $ ordinal 30
        "tr\x00edochad\x00fa" -> Just $ ordinal 30
        "triochad\x00fa" -> Just $ ordinal 30
        "daichead\x00fa" -> Just $ ordinal 40
        "daicheadu" -> Just $ ordinal 40
        "caogadu" -> Just $ ordinal 50
        "caogad\x00fa" -> Just $ ordinal 50
        "seascadu" -> Just $ ordinal 60
        "seascad\x00fa" -> Just $ ordinal 60
        "seachtodu" -> Just $ ordinal 70
        "seachtod\x00fa" -> Just $ ordinal 70
        "seacht\x00f3d\x00fa" -> Just $ ordinal 70
        "seacht\x00f3du" -> Just $ ordinal 70
        "ocht\x00f3du" -> Just $ ordinal 80
        "ochtodu" -> Just $ ordinal 80
        "t-ochtodu" -> Just $ ordinal 80
        "t-ocht\x00f3d\x00fa" -> Just $ ordinal 80
        "t-ochtod\x00fa" -> Just $ ordinal 80
        "ocht\x00f3d\x00fa" -> Just $ ordinal 80
        "t-ocht\x00f3du" -> Just $ ordinal 80
        "ochtod\x00fa" -> Just $ ordinal 80
        "n\x00f3chad\x00fa" -> Just $ ordinal 90
        "n\x00f3chadu" -> Just $ ordinal 90
        "nochad\x00fa" -> Just $ ordinal 90
        "nochadu" -> Just $ ordinal 90
        "c\x00e9ad\x00fa" -> Just $ ordinal 100
        "cead\x00fa" -> Just $ ordinal 100
        "ceadu" -> Just $ ordinal 100
        "c\x00e9adu" -> Just $ ordinal 100
        "miliu" -> Just $ ordinal 1000
        "mili\x00fa" -> Just $ ordinal 1000
        "milliun\x00fa" -> Just $ ordinal 1000000
        "milliunu" -> Just $ ordinal 1000000
        "milli\x00fanu" -> Just $ ordinal 1000000
        "milli\x00fan\x00fa" -> Just $ ordinal 1000000
        _ -> Nothing
      _ -> Nothing
  }

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern =
    [ regex "0*(\\d+) ?(adh|a|d|\x00fa|u)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> ordinal <$> parseInt match
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleOrdinalDigits
  , ruleOrdinalsChadDaraEtc
  ]
