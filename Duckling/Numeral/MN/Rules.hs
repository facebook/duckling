-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Numeral.MN.Rules
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
  [ ( "хорь", 20)
  , ( "гуч", 30)
  , ( "дөч", 40)
  , ( "тавь", 50)
  , ( "жар", 60)
  , ( "дал", 70)
  , ( "ная", 80)
  , ( "ер", 90)
  ]

ruleNumeral :: Rule
ruleNumeral = Rule
  {  name = "integer (20..90)"
  , pattern =
    [ regex "(хорь|гуч|дөч|тавь|жар|дал|ная|ер)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) tensMap >>= integer
      _ -> Nothing
  }

elevenToNineteenMap :: HashMap Text Integer
elevenToNineteenMap = HashMap.fromList
  [ ( "арван нэг", 11 )
  , ( "арван хоёр", 12 )
  , ( "арван гурав", 13 )
  , ( "арван дөрөв", 14 )
  , ( "арван тав", 15 )
  , ( "арван зургаа", 16 )
  , ( "арван долоо", 17 )
  , ( "арван найм", 18 )
  , ( "арван ес", 19 )
  ]

ruleElevenToNineteen :: Rule
ruleElevenToNineteen = Rule
  { name = "number (11..19)"
  , pattern =
    [ regex "(арван нэг|арван хоёр|арван гурав|арван дөрөв|арван тав|арван зургаа|арван долоо|арван найм|арван ес)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) elevenToNineteenMap >>= integer
      _ -> Nothing
  }

twentyoneToTwentynineMap :: HashMap Text Integer
twentyoneToTwentynineMap = HashMap.fromList
  [ ( "хорин нэг", 21 )
  , ( "хорин хоёр", 22 )
  , ( "хорин гурав", 23 )
  , ( "хорин дөрөв", 24 )
  , ( "хорин тав", 25 )
  , ( "хорин зургаа", 26 )
  , ( "хорин долоо", 27 )
  , ( "хорин найм", 28 )
  , ( "хорин ес", 29 )
  ]

ruleTwentyoneToTwentynine :: Rule
ruleTwentyoneToTwentynine = Rule
  { name = "number (21..29)"
  , pattern =
    [ regex "(хорин нэг|хорин хоёр|хорин гурав|хорин дөрөв|хорин тав|хорин зургаа|хорин долоо|хорин найм|хорин ес)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) twentyoneToTwentynineMap >>= integer
      _ -> Nothing
  }


hundredsMap :: HashMap Text Integer
hundredsMap = HashMap.fromList
  [ ( "зуу", 100)
  , ( "хоёр зуу", 200)
  , ( "гурван зуу", 300)
  , ( "дөрвөн зуу", 400)
  , ( "таван зуу", 500)
  , ( "зургаан зуу", 600)
  , ( "долоон зуу", 700)
  , ( "найман зуу", 800)
  , ( "есөн зуу", 900)
  ]

ruleInteger6 :: Rule
ruleInteger6 = Rule
  { name = "integer (100..900)"
  , pattern =
    [ regex "(зуу|хоёр зуу|гурван зуу|дөрвөн зуу|таван зуу|зургаан зуу|долоон зуу|найман зуу|есөн зуу)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) hundredsMap >>= integer
      _ -> Nothing
  }

ruleIntegerWithThousandsSeparator :: Rule
ruleIntegerWithThousandsSeparator = Rule
  { name = "integer with thousands separator ,"
  , pattern =
    [ regex "(\\d{1,3}(,\\d\\d\\d){1,5})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDouble (Text.replace "," Text.empty match) >>= double
      _ -> Nothing
  }
rules :: [Rule]
rules =
  [ ruleNumeral
  , ruleElevenToNineteen
  , ruleTwentyoneToTwentynine
  , ruleInteger6
  , ruleIntegerWithThousandsSeparator
  ]
