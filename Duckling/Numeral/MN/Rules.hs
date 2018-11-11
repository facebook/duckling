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
    [ regex "(хорь|гуч|дөч|тавь|жар|дал|ная|ер|хорин|гучин|дөчин|тавин|жаран|далан|наян|ерэн)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) ruleNumeralMap >>= integer
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
ruleInteger :: Rule
ruleInteger = Rule
  { name = "integer 0"
  , pattern =
    [ regex "(нойл|ноль|тэг)"
    ]
  , prod = \_ -> integer 0
  }
ruleInteger2 :: Rule
ruleInteger2 = Rule
  { name = "integer 1"
  , pattern =
    [ regex "(нэг|ганц)"
    ]
  , prod = \_ -> integer 1
  }

ruleInteger3 :: Rule
ruleInteger3 = Rule
  { name = "integer 2"
  , pattern =
    [ regex "(хоёр|хос)"
    ]
  , prod = \_ -> integer 2
  }

threeToNineteenMap:: HashMap Text Integer
threeToNineteenMap = HashMap.fromList
  [ ( "гурав", 3)
  , ( "дөрөв", 4)
  , ( "тав", 5)
  , ( "зургаа", 6)
  , ( "долоо", 7)
  , ( "найм", 8)
  , ( "ес", 9)
  , ( "арав", 10)
  , ( "арваннэг", 11)
  , ( "арванхоёр", 12)
  , ( "арвангурав", 13)
  , ( "арвандөрөв", 14)
  , ( "арвантав", 15)
  , ( "арванзургаа", 16)
  , ( "арвандолоо", 17)
  , ( "арваннайм", 18)
  , ( "арванес", 19)
  ]
ruleInteger4 :: Rule
ruleInteger4 = Rule
  { name = "integer (3..19)"
  , pattern =
    [ regex "(гурав|дөрөв|тав|зургаа|долоо|найм|ес|арав|арваннэг|арванхоёр|арвангурав|арвандөрөв|арвантав|арванзургаа|арвандолоо|арваннайм|арванес)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) threeToNineteenMap >>= integer
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

ruleInteger7 :: Rule
ruleInteger7 = Rule
  { name = "integer 21..99"
  , pattern =
    [ oneOf [70, 20, 60, 50, 40, 90, 30, 80]
    , numberBetween 1 10
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v1}:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleInteger8 :: Rule
ruleInteger8 = Rule
  { name = "integer 101..999"
  , pattern =
    [ oneOf [300, 600, 500, 100, 800, 200, 900, 700, 400]
    , numberBetween 1 100
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v1}:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleNumeralDotNumeral :: Rule
ruleNumeralDotNumeral = Rule
  { name = "number dot number"
  , pattern =
    [ dimension Numeral
    , regex "цэг"
    , Predicate $ not . hasGrain
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral nd1:_:Token Numeral nd2:_) ->
        double $ TNumeral.value nd1 + decimalsToDouble (TNumeral.value nd2)
      _ -> Nothing
  }

ruleNumeralsSuffixesKMG :: Rule
ruleNumeralsSuffixesKMG = Rule
  { name = "numbers suffixes (K, M, G)"
  , pattern =
    [ dimension Numeral
    , regex "((к|м|г)|(К|М|Г))(?=[\\W\\$€]|$)"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
         "к" -> double $ v * 1e3
         "К" -> double $ v * 1e3
         "м" -> double $ v * 1e6
         "М" -> double $ v * 1e6
         "г" -> double $ v * 1e9
         "Г" -> double $ v * 1e9
         _   -> Nothing
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleNumeral
  , ruleElevenToNineteen
  , ruleInteger
  , ruleInteger2
  , ruleInteger3
  , ruleInteger4
  , ruleInteger6
  , ruleInteger7
  , ruleInteger8
  , ruleIntegerWithThousandsSeparator
  , ruleNumeralDotNumeral
  , ruleNumeralsSuffixesKMG
  ]
