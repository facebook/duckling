-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.PL.Rules
  ( rules ) where

import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Helpers
import Duckling.Ordinal.Types (OrdinalData (..))
import qualified Duckling.Ordinal.Types as TOrdinal
import Duckling.Regex.Types
import Duckling.Types

ruleThOrdinalNoSpace :: Rule
ruleThOrdinalNoSpace = Rule
  { name = "24th ordinal no space"
  , pattern =
    [ regex "dwudziest(ym|y|ego|emu|(a|ą)|ej)czwart(y|ego|emu|ym|(a|ą)|ej)"
    ]
  , prod = \_ -> Just $ ordinal 24
  }

ruleThOrdinal16 :: Rule
ruleThOrdinal16 = Rule
  { name = "31-39th ordinal"
  , pattern =
    [ regex "trzydziest(ym|y|ego|emu|(a|ą)|ej)( |-)?"
    , dimension Ordinal
    ]
  , prod = \tokens -> case tokens of
      (_:Token Ordinal OrdinalData{TOrdinal.value = v}:_) ->
        Just . ordinal $ 30 + v
      _ -> Nothing
  }

ruleThOrdinal3 :: Rule
ruleThOrdinal3 = Rule
  { name = "10th ordinal"
  , pattern =
    [ regex "dziesi(a|ą)t(ym|y|ego|emu|(a|ą)|ej)"
    ]
  , prod = \_ -> Just $ ordinal 10
  }

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern =
    [ regex "0*(\\d+)( |-)?(szy|sza|szym|ego|go|szego|gi(ego|ej)?|st(a|y|ej)|t(ej|y|ego)|ci(ego)?)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> ordinal <$> parseInt match
      _ -> Nothing
  }

ruleThOrdinal8 :: Rule
ruleThOrdinal8 = Rule
  { name = "15th ordinal"
  , pattern =
    [ regex "pi(e|ę)tnast(ym|y|ego|emu|(a|ą)|ej)"
    ]
  , prod = \_ -> Just $ ordinal 15
  }

ruleThOrdinal13 :: Rule
ruleThOrdinal13 = Rule
  { name = "20th ordinal"
  , pattern =
    [ regex "dwudziest(ym|y|ego|emu|(a|ą)|ej)"
    ]
  , prod = \_ -> Just $ ordinal 20
  }

ruleThOrdinal4 :: Rule
ruleThOrdinal4 = Rule
  { name = "11th ordinal"
  , pattern =
    [ regex "jedenast(ym|y|ego|emu|(a|ą)|ej)"
    ]
  , prod = \_ -> Just $ ordinal 11
  }

ruleFifthOrdinal :: Rule
ruleFifthOrdinal = Rule
  { name = "fifth ordinal"
  , pattern =
    [ regex "pi(a|ą)t(y|ego|emu|m|(a|ą)|ej)"
    ]
  , prod = \_ -> Just $ ordinal 5
  }

ruleThOrdinal11 :: Rule
ruleThOrdinal11 = Rule
  { name = "18th ordinal"
  , pattern =
    [ regex "osiemnast(ym|y|ego|emu|(a|ą)|ej)"
    ]
  , prod = \_ -> Just $ ordinal 18
  }

ruleSecondOrdinal :: Rule
ruleSecondOrdinal = Rule
  { name = "second ordinal"
  , pattern =
    [ regex "drugi?(ego|emu|m|(a|ą)|ej)?"
    ]
  , prod = \_ -> Just $ ordinal 2
  }

ruleNdOrdinalNoSpace :: Rule
ruleNdOrdinalNoSpace = Rule
  { name = "22nd ordinal no space"
  , pattern =
    [ regex "dwudziest(ym|y|ego|emu|(a|ą)|ej)drugi?(ego|emu|m|(a|ą)|ej)?"
    ]
  , prod = \_ -> Just $ ordinal 22
  }

ruleSeventhOrdinal :: Rule
ruleSeventhOrdinal = Rule
  { name = "seventh ordinal"
  , pattern =
    [ regex "si(o|ó)dm(y|ego|emu|m|(a|ą)|ej)"
    ]
  , prod = \_ -> Just $ ordinal 7
  }

ruleStOrdinalNoSpace :: Rule
ruleStOrdinalNoSpace = Rule
  { name = "21st ordinal no space"
  , pattern =
    [ regex "dwudziest(ym|y|ego|emu|(a|ą)|ej)pierw?sz(y|ego|emu|m|(a|ą)|ej)"
    ]
  , prod = \_ -> Just $ ordinal 21
  }

ruleThOrdinal7 :: Rule
ruleThOrdinal7 = Rule
  { name = "14th ordinal"
  , pattern =
    [ regex "czternast(ym|y|ego|emu|(a|ą)|ej)"
    ]
  , prod = \_ -> Just $ ordinal 14
  }

ruleThOrdinal2 :: Rule
ruleThOrdinal2 = Rule
  { name = "9th ordinal"
  , pattern =
    [ regex "dziewi(a|ą)t(ym|y|ego|em|emu|(a|ą)|ej)"
    ]
  , prod = \_ -> Just $ ordinal 9
  }

ruleThOrdinal9 :: Rule
ruleThOrdinal9 = Rule
  { name = "16th ordinal"
  , pattern =
    [ regex "szesnast(ym|y|ego|emu|(a|ą)|ej)"
    ]
  , prod = \_ -> Just $ ordinal 16
  }

ruleThOrdinal :: Rule
ruleThOrdinal = Rule
  { name = "8th ordinal"
  , pattern =
    [ regex "(o|ó|Ó)sm(y|ego|emu|m|(a|ą)|ej)"
    ]
  , prod = \_ -> Just $ ordinal 8
  }

ruleThOrdinal14 :: Rule
ruleThOrdinal14 = Rule
  { name = "21-29th ordinal"
  , pattern =
    [ regex "dwudziest(ym|y|ego|emu|(a|ą)|ej)( |-)?"
    , dimension Ordinal
    ]
  , prod = \tokens -> case tokens of
      (_:Token Ordinal OrdinalData{TOrdinal.value = v}:_) ->
        Just . ordinal $ 20 + v
      _ -> Nothing
  }

ruleThOrdinal10 :: Rule
ruleThOrdinal10 = Rule
  { name = "17th ordinal"
  , pattern =
    [ regex "siedemnast(ym|y|ego|emu|(a|ą)|ej)"
    ]
  , prod = \_ -> Just $ ordinal 17
  }

ruleRdOrdinalNoSpace :: Rule
ruleRdOrdinalNoSpace = Rule
  { name = "23rd ordinal no space"
  , pattern =
    [ regex "dwudziest(ym|y|ego|emu|(a|ą)|ej)trzeci(ego|ch|emu|m|mi|ej|(a|ą))?"
    ]
  , prod = \_ -> Just $ ordinal 23
  }

ruleThOrdinal5 :: Rule
ruleThOrdinal5 = Rule
  { name = "12th ordinal"
  , pattern =
    [ regex "dwunast(ym|y|ego|emu|(a|ą)|ej)"
    ]
  , prod = \_ -> Just $ ordinal 12
  }

ruleThOrdinal6 :: Rule
ruleThOrdinal6 = Rule
  { name = "13th ordinal"
  , pattern =
    [ regex "trzynast(ym|y|ego|emu|(a|ą)|ej)"
    ]
  , prod = \_ -> Just $ ordinal 13
  }

ruleFirstOrdinal :: Rule
ruleFirstOrdinal = Rule
  { name = "first ordinal"
  , pattern =
    [ regex "pierw?sz(y|ego|emu|m|(a|ą)|ej)"
    ]
  , prod = \_ -> Just $ ordinal 1
  }

ruleSixthOrdinal :: Rule
ruleSixthOrdinal = Rule
  { name = "sixth ordinal"
  , pattern =
    [ regex "sz(o|ó)st(y|ego|emu|m|(a|ą)|ej)"
    ]
  , prod = \_ -> Just $ ordinal 6
  }

ruleFourthOrdinal :: Rule
ruleFourthOrdinal = Rule
  { name = "fourth ordinal"
  , pattern =
    [ regex "czwart(y|ego|emu|ym|(a|ą)|ej)"
    ]
  , prod = \_ -> Just $ ordinal 4
  }

ruleThOrdinal15 :: Rule
ruleThOrdinal15 = Rule
  { name = "30th ordinal"
  , pattern =
    [ regex "trzydziest(ym|y|ego|emu|(a|ą)|ej)"
    ]
  , prod = \_ -> Just $ ordinal 30
  }

ruleThOrdinal12 :: Rule
ruleThOrdinal12 = Rule
  { name = "19th ordinal"
  , pattern =
    [ regex "dziewi(ę|e)tnast(ym|y|ego|emu|(a|ą)|ej)"
    ]
  , prod = \_ -> Just $ ordinal 19
  }

ruleThirdOrdinal :: Rule
ruleThirdOrdinal = Rule
  { name = "third ordinal"
  , pattern =
    [ regex "trzeci(ego|ch|emu|m|mi|ej|(a|ą))?"
    ]
  , prod = \_ -> Just $ ordinal 3
  }

rules :: [Rule]
rules =
  [ ruleFifthOrdinal
  , ruleFirstOrdinal
  , ruleFourthOrdinal
  , ruleNdOrdinalNoSpace
  , ruleOrdinalDigits
  , ruleRdOrdinalNoSpace
  , ruleSecondOrdinal
  , ruleSeventhOrdinal
  , ruleSixthOrdinal
  , ruleStOrdinalNoSpace
  , ruleThOrdinal
  , ruleThOrdinal10
  , ruleThOrdinal11
  , ruleThOrdinal12
  , ruleThOrdinal13
  , ruleThOrdinal14
  , ruleThOrdinal15
  , ruleThOrdinal16
  , ruleThOrdinal2
  , ruleThOrdinal3
  , ruleThOrdinal4
  , ruleThOrdinal5
  , ruleThOrdinal6
  , ruleThOrdinal7
  , ruleThOrdinal8
  , ruleThOrdinal9
  , ruleThOrdinalNoSpace
  , ruleThirdOrdinal
  ]
