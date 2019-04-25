-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.DE.Rules
  ( rules ) where

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
    , regex "(erste(r|s)?|zweite(r|s)?|dritte(r|s)?|vierte(r|s)?|fünfte(r|s)?|sechste(r|s)?|siebte(r|s)?|achte(r|s)?|neunte(r|s)?|zehnte(r|s)?|elfte(r|s)?|zwölfte(r|s)?|dreizente(r|s)?|vierzehnte(r|s)?|fünfzehnte(r|s)?|sechzente(r|s)?|siebzehnte(r|s)?|achtzehnte(r|s)?|neunzehnte(r|s)?|zwanzigste(r|s)?|einundzwanzigste(r|s)?|zweiundzwanzigste(r|s)?|dreiundzwanzigste(r|s)?|vierundzwanzigste(r|s)?|fünfundzwanzigste(r|s)?|sechsundzwanzigste(r|s)?|siebenundzwanzigste(r|s)?|achtundzwanzigste(r|s)?|neunundzwanzigste(r|s)?|dreißigste(r|s)?|einunddreißigste(r|s)?)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "erstes" -> Just $ ordinal 1
        "erster" -> Just $ ordinal 1
        "erste" -> Just $ ordinal 1
        "zweiter" -> Just $ ordinal 2
        "zweite" -> Just $ ordinal 2
        "zweites" -> Just $ ordinal 2
        "drittes" -> Just $ ordinal 3
        "dritte" -> Just $ ordinal 3
        "dritter" -> Just $ ordinal 3
        "viertes" -> Just $ ordinal 4
        "vierte" -> Just $ ordinal 4
        "vierter" -> Just $ ordinal 4
        "fünftes" -> Just $ ordinal 5
        "fünfter" -> Just $ ordinal 5
        "fünfte" -> Just $ ordinal 5
        "sechste" -> Just $ ordinal 6
        "sechstes" -> Just $ ordinal 6
        "sechster" -> Just $ ordinal 6
        "siebtes" -> Just $ ordinal 7
        "siebte" -> Just $ ordinal 7
        "siebter" -> Just $ ordinal 7
        "achtes" -> Just $ ordinal 8
        "achte" -> Just $ ordinal 8
        "achter" -> Just $ ordinal 8
        "neuntes" -> Just $ ordinal 9
        "neunter" -> Just $ ordinal 9
        "neunte" -> Just $ ordinal 9
        "zehnte" -> Just $ ordinal 10
        "zehnter" -> Just $ ordinal 10
        "zehntes" -> Just $ ordinal 10
        "elfte" -> Just $ ordinal 11
        "elfter" -> Just $ ordinal 11
        "elftes" -> Just $ ordinal 11
        "zwölfte" -> Just $ ordinal 12
        "zwölfter" -> Just $ ordinal 12
        "zwölftes" -> Just $ ordinal 12
        "dreizehnte" -> Just $ ordinal 13
        "dreizehnter" -> Just $ ordinal 13
        "dreizehntes" -> Just $ ordinal 13
        "vierzehnte" -> Just $ ordinal 14
        "vierzehnter" -> Just $ ordinal 14
        "vierzehntes" -> Just $ ordinal 14
        "fünfzehnte" -> Just $ ordinal 15
        "fünfzehnter" -> Just $ ordinal 15
        "fünfzehntes" -> Just $ ordinal 15
        "sechzehnte" -> Just $ ordinal 16
        "sechzehnter" -> Just $ ordinal 16
        "sechzehntes" -> Just $ ordinal 16
        "siebzehnte" -> Just $ ordinal 17
        "siebzehnter" -> Just $ ordinal 17
        "siebzehntes" -> Just $ ordinal 17
        "achtzehnte" -> Just $ ordinal 18
        "achtzehnter" -> Just $ ordinal 18
        "achtzehntes" -> Just $ ordinal 18
        "neunzehnte" -> Just $ ordinal 19
        "neunzehnter" -> Just $ ordinal 19
        "neunzehntes" -> Just $ ordinal 19
        "zwanzigste" -> Just $ ordinal 20
        "zwanzigster" -> Just $ ordinal 20
        "zwanzigster" -> Just $ ordinal 20
        "zwanzigstes" -> Just $ ordinal 20
        "einundzwanzigste" -> Just $ ordinal 21
        "einundzwanzigster" -> Just $ ordinal 21
        "einundzwanzigstes" -> Just $ ordinal 21
        "zweiundzwanzigste" -> Just $ ordinal 22
        "zweiundzwanzigster" -> Just $ ordinal 22
        "zweiundzwanzigstes" -> Just $ ordinal 22
        "dreiundzwanzigste" -> Just $ ordinal 23
        "dreiundzwanzigster" -> Just $ ordinal 23
        "dreiundzwanzigstes" -> Just $ ordinal 23
        "vierundzwanzigste" -> Just $ ordinal 24
        "vierundzwanzigster" -> Just $ ordinal 24
        "vierundzwanzigstes" -> Just $ ordinal 24
        "fünfundzwanzigste" -> Just $ ordinal 25
        "fünfundzwanzigster" -> Just $ ordinal 25
        "fünfundzwanzigstes" -> Just $ ordinal 25
        "sechsundzwanzigste" -> Just $ ordinal 26
        "sechsundzwanzigster" -> Just $ ordinal 26
        "sechsundzwanzigstes" -> Just $ ordinal 26
        "siebenundzwanzigste" -> Just $ ordinal 27
        "siebenundzwanzigster" -> Just $ ordinal 27
        "siebenundzwanzigstes" -> Just $ ordinal 27
        "achtundzwanzigste" -> Just $ ordinal 28
        "achtundzwanzigster" -> Just $ ordinal 28
        "achtundzwanzigstes" -> Just $ ordinal 28
        "neunundzwanzigste" -> Just $ ordinal 29
        "neunundzwanzigster"  -> Just $ ordinal 29
        "neunundzwanzigstes"  -> Just $ ordinal 29
        "dreißigste" -> Just $ ordinal 30
        "dreißigster" -> Just $ ordinal 30
        "dreißigstes" -> Just $ ordinal 30
        "einunddreißigste" -> Just $ ordinal 31
        "einunddreißigster"  -> Just $ ordinal 31
        "einunddreißigstes" -> Just $ ordinal 31
        _ -> Nothing
      _ -> Nothing
  }

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern =
    [ regex "(?<!\\d|\\.)0*(\\d+)(\\.(?!\\d)| ?(te(n|r|s)?)|(ste(n|r|s)?))"
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
  , ruleOrdinalsFirstth
  ]
