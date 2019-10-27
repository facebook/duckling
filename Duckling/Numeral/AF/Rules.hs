-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Numeral.AF.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral

ruleNumeralMap :: HashMap Text Integer
ruleNumeralMap = HashMap.fromList
  [ ( "nul", 0 )
  , ( "zero", 0 )
  , ( "een", 1 )
  , ( "twee", 2 )
  , ( "drie", 3 )
  , ( "vier", 4 )
  , ( "vyf", 5)
  , ( "ses", 6)
  , ( "sewe", 7)
  , ( "agt", 8)
  , ( "nege", 9)
  , ( "tien", 10)
  ]

ruleNumeral :: Rule
ruleNumeral = Rule
  { name = "number (0..10)"
  , pattern =
    [ regex "(nul|zero|een|twee|drie|vier|vyf|ses|sewe|agt|nege|tien)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) ruleNumeralMap >>= integer
      _ -> Nothing
  }

elevenToNineteenMap :: HashMap Text Integer
elevenToNineteenMap = HashMap.fromList
  [ ( "elf", 11 )
  , ( "twaalf", 12 )
  , ( "dertien", 13 )
  , ( "viertien", 14 )
  , ( "vyftien", 15 )
  , ( "sestien", 16 )
  , ( "sewentien", 17 )
  , ( "agtien", 18 )
  , ( "negentien", 19 )
  ]

ruleElevenToNineteen :: Rule
ruleElevenToNineteen = Rule
  { name = "number (11..19)"
  , pattern =
    [ regex "(elf|twaalf|dertien|viertien|vyftien|sestien|sewentien|agtien|negentien)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) elevenToNineteenMap >>= integer
      _ -> Nothing
  }

twentyoneToTwentynineMap :: HashMap Text Integer
twentyoneToTwentynineMap = HashMap.fromList
  [ ( "een en twintig", 21 )
  , ( "twee en twintig", 22 )
  , ( "drie en twintig", 23 )
  , ( "vier en twintig", 24 )
  , ( "vyf en twintig", 25 )
  , ( "ses en twintig", 26 )
  , ( "sewe en twintig", 27 )
  , ( "agt en twintig", 28 )
  , ( "nege en twintig", 29 )
  ]

ruleTwentyoneToTwentynine :: Rule
ruleTwentyoneToTwentynine = Rule
  { name = "number (21..29)"
  , pattern =
    [ regex "(een en twintig|twee en twintig|drie en twintig|vier en twintig|vyf en twintig|ses en twintig|sewe en twintig|agt en twintig|nege en twintig)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) twentyoneToTwentynineMap >>= integer
      _ -> Nothing
  }

tensMap :: HashMap Text Integer
tensMap = HashMap.fromList
  [ ( "twintig", 20 )
  , ( "dertig", 30 )
  , ( "viertig", 40 )
  , ( "vyftig", 50 )
  , ( "sestig", 60 )
  , ( "sewentig", 70 )
  , ( "tagtig", 80 )
  , ( "negentig", 90 )
  ]
ruleTens :: Rule
ruleTens = Rule
  { name = "integer (20,30..90)"
  , pattern =
    [ regex "(twintig|dertig|viertig|vyftig|sestig|sewentig|tagtig|negentig)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) tensMap >>= integer
      _ -> Nothing
  }

ruleCompositeTens :: Rule
ruleCompositeTens = Rule
  { name = "integer ([3-9][1-9])"
  , pattern =
    [ regex "(een|twee|drie|vier|vyf|ses|sewe|agt|nege) en (dertig|viertig|vyftig|sestig|sewentig|tagtig|negentig)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        v1 <- HashMap.lookup (Text.toLower m1) tensMap
        v2 <- HashMap.lookup (Text.toLower m2) ruleNumeralMap
        integer $ v1 + v2
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleNumeral
  , ruleElevenToNineteen
  , ruleTwentyoneToTwentynine
  , ruleTens
  , ruleCompositeTens
  ]
