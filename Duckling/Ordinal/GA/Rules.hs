-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


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
    [ regex "(ch(é|e)ad|aon(ú|u)|t-aon(ú|u)|dara|tr(í|i)(ú|u)|ceathr(ú|u)|c(ú|u)igi(ú|u)|s(é|e)(ú|u)|seacht(ú|u)|ocht(ú|u)|t-ocht(ú|u)|nao(ú|u)|deichi(ú|u)|fichi(ú|u)|tr(í|i)ochad(ú|u)|daichead(ú|u)|caogad(ú|u)|seascad(ú|u)|seacht(ó|o)d(ú|u)|ocht(ó|o)d(ú|u)|t-ocht(ó|o)d(ú|u)|n(ó|o)chad(ú|u)|c(é|e)ad(ú|u)|mili(ú|u)|milli(ú|u)n(ú|u))"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "t-aonu" -> Just $ ordinal 1
        "aonu" -> Just $ ordinal 1
        "aonú" -> Just $ ordinal 1
        "chéad" -> Just $ ordinal 1
        "chead" -> Just $ ordinal 1
        "t-aonú" -> Just $ ordinal 1
        "dara" -> Just $ ordinal 2
        "triú" -> Just $ ordinal 3
        "tríu" -> Just $ ordinal 3
        "tríú" -> Just $ ordinal 3
        "triu" -> Just $ ordinal 3
        "ceathrú" -> Just $ ordinal 4
        "ceathru" -> Just $ ordinal 4
        "cúigiu" -> Just $ ordinal 5
        "cúigiú" -> Just $ ordinal 5
        "cuigiu" -> Just $ ordinal 5
        "cuigiú" -> Just $ ordinal 5
        "séu" -> Just $ ordinal 6
        "séú" -> Just $ ordinal 6
        "seu" -> Just $ ordinal 6
        "seú" -> Just $ ordinal 6
        "seachtu" -> Just $ ordinal 7
        "seachtú" -> Just $ ordinal 7
        "t-ochtú" -> Just $ ordinal 8
        "ochtu" -> Just $ ordinal 8
        "t-ochtu" -> Just $ ordinal 8
        "ochtú" -> Just $ ordinal 8
        "naou" -> Just $ ordinal 9
        "naoú" -> Just $ ordinal 9
        "deichiu" -> Just $ ordinal 10
        "deichiú" -> Just $ ordinal 10
        "fichiu" -> Just $ ordinal 20
        "fichiú" -> Just $ ordinal 20
        "tríochadu" -> Just $ ordinal 30
        "triochadu" -> Just $ ordinal 30
        "tríochadú" -> Just $ ordinal 30
        "triochadú" -> Just $ ordinal 30
        "daicheadú" -> Just $ ordinal 40
        "daicheadu" -> Just $ ordinal 40
        "caogadu" -> Just $ ordinal 50
        "caogadú" -> Just $ ordinal 50
        "seascadu" -> Just $ ordinal 60
        "seascadú" -> Just $ ordinal 60
        "seachtodu" -> Just $ ordinal 70
        "seachtodú" -> Just $ ordinal 70
        "seachtódú" -> Just $ ordinal 70
        "seachtódu" -> Just $ ordinal 70
        "ochtódu" -> Just $ ordinal 80
        "ochtodu" -> Just $ ordinal 80
        "t-ochtodu" -> Just $ ordinal 80
        "t-ochtódú" -> Just $ ordinal 80
        "t-ochtodú" -> Just $ ordinal 80
        "ochtódú" -> Just $ ordinal 80
        "t-ochtódu" -> Just $ ordinal 80
        "ochtodú" -> Just $ ordinal 80
        "nóchadú" -> Just $ ordinal 90
        "nóchadu" -> Just $ ordinal 90
        "nochadú" -> Just $ ordinal 90
        "nochadu" -> Just $ ordinal 90
        "céadú" -> Just $ ordinal 100
        "ceadú" -> Just $ ordinal 100
        "ceadu" -> Just $ ordinal 100
        "céadu" -> Just $ ordinal 100
        "miliu" -> Just $ ordinal 1000
        "miliú" -> Just $ ordinal 1000
        "milliunú" -> Just $ ordinal 1000000
        "milliunu" -> Just $ ordinal 1000000
        "milliúnu" -> Just $ ordinal 1000000
        "milliúnú" -> Just $ ordinal 1000000
        _ -> Nothing
      _ -> Nothing
  }

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern =
    [ regex "0*(\\d+) ?(adh|a|d|ú|u)"
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
