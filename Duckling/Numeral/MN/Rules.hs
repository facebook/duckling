-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.MN.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.Text (Text)
import Prelude
import Data.String
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral

ruleNumeralsPrefixWithNegativeOrMinus :: Rule
ruleNumeralsPrefixWithNegativeOrMinus = Rule
  { name = "numbers prefix with -, negative or minus"
  , pattern =
    [ regex "-|хасах|сөрөг"
    , Predicate isPositive
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral NumeralData{TNumeral.value = v}:_) ->
        double $ v * (- 1)
      _ -> Nothing
  }

ruleFew :: Rule
ruleFew = Rule
  { name = "few"
  , pattern =
    [ regex "хэдхэн"
    ]
  , prod = \_ -> integer 3
  }

ruleTen :: Rule
ruleTen = Rule
  { name = "ten"
  , pattern =
    [ regex "арав"
    ]
  , prod = \_ -> integer 10 >>= withGrain 1
  }

ruleDecimalWithThousandsSeparator :: Rule
ruleDecimalWithThousandsSeparator = Rule
  { name = "decimal with thousands separator"
  , pattern =
    [ regex "(\\d+(\\.\\d\\d\\d)+\\,\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> let fmt = Text.replace "," "." $ Text.replace "." Text.empty match
        in parseDouble fmt >>= double
      _ -> Nothing
  }


-- TODO: Single-word composition (#110)
ruleInteger3 :: Rule
ruleInteger3 = Rule
  { name = "integer ([2-9][1-9])"
  , pattern =
    [ regex "(хорин|гучин|дөчин|тавин|жаран|далан|наян|ерэн) ?(нэг|хоёр|гурав|дөрөв|тав|зургаа|долоо|найм|ес)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        v1 <- case Text.toLower m1 of
          "хорин" -> Just 20
          "гучин" -> Just 30
          "дөчин" -> Just 40
          "тавин" -> Just 50
          "жаран" -> Just 60
          "далан" -> Just 70
          "наян" -> Just 80
          "ерэн" -> Just 90
          _ -> Nothing
        v2 <- case Text.toLower m2 of
          "нэг" -> Just 1
          "хоёр" -> Just 2
          "гурав" -> Just 3
          "дөрөв" -> Just 4
          "тав" -> Just 5
          "зургаа" -> Just 6
          "долоо" -> Just 7
          "найм" -> Just 8
          "ес" -> Just 9
          _ -> Nothing
        integer $ v1 + v2
      _ -> Nothing
  }

ruleNumeralsUnd :: Rule
ruleNumeralsUnd = Rule
  { name = "numbers und"
  , pattern =
    [ oneOf [20, 30 .. 90]
    , numberBetween 1 10
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v1}:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleMultiply :: Rule
ruleMultiply = Rule
  { name = "compose by multiplication"
  , pattern =
    [ dimension Numeral
    , Predicate isMultipliable
    ]
  , prod = \tokens -> case tokens of
      (token1:token2:_) -> multiply token1 token2
      _ -> Nothing
  }

ruleIntersect :: Rule
ruleIntersect = Rule
  { name = "intersect"
  , pattern =
    [ Predicate hasGrain
    , Predicate $ and . sequence [not . isMultipliable, isPositive]
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = val1, TNumeral.grain = Just g}:
       Token Numeral NumeralData{TNumeral.value = val2}:
       _) | (10 ** fromIntegral g) > val2 -> double $ val1 + val2
      _ -> Nothing
  }

ruleNumeralsSuffixesKMG :: Rule
ruleNumeralsSuffixesKMG = Rule
  { name = "numbers suffixes (K, M, G)"
  , pattern =
    [ dimension Numeral
    , regex "([kmg])(?=[\\W\\$€]|$)"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
         "k" -> double $ v * 1e3
         "m" -> double $ v * 1e6
         "g" -> double $ v * 1e9
         _ -> Nothing
      _ -> Nothing
  }

ruleCouple :: Rule
ruleCouple = Rule
  { name = "couple"
  , pattern =
    [ regex "хос"
    ]
  , prod = \_ -> integer 2
  }


rulePowersOfTen :: Rule
rulePowersOfTen = Rule
  { name = "powers of tens"
  , pattern =
    [ regex "(зуу?|мянга?|сая?)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "зуу"   -> double 1e2 >>= withGrain 2 >>= withMultipliable
        "зуун"  -> double 1e2 >>= withGrain 2 >>= withMultipliable
        "мянга"   -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "мянган"  -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "сая"   -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "саяын" -> double 1e6 >>= withGrain 6 >>= withMultipliable
        _           -> Nothing
      _ -> Nothing
  }

zeroNineteenMap :: HashMap Text Integer
zeroNineteenMap = HashMap.fromList
  [ ("нуль", 0)
  , ("тэг", 0)
  , ("нойл", 0)
  , ("нэг", 1)
  , ("ганц", 1)
  , ("хоёр", 2)
  , ("гурав", 3)
  , ("дөрөв", 4)
  , ("тав", 5)
  , ("зургаа", 6)
  , ("долоо", 7)
  , ("найм", 8)
  , ("ес", 9)
  , ("арван", 10)
  , ("арав", 10)
  , ("арван нэг", 11)
  , ("арван хоёр", 12)
  , ("арван гурав", 13)
  , ("арван дөрөв", 14)
  , ("арван тав", 15)
  , ("арван зургаа", 16)
  , ("арван долоо", 17)
  , ("арван найм", 18)
  , ("арван ес", 19)
  ]

-- TODO: Single-word composition (#110)
ruleZeroToNineteen :: Rule
ruleZeroToNineteen = Rule
  { name = "integer (0..19)"
  , pattern =
    [ regex "(нуль|тэг|нойл|нэг|ганц|хоёр|гурав|дөрөв|тав|зургаа|долоо|найм|ес|арван нэг|арван хоёр|арван гурав|арван дөрөв|арван тав|арван зургаа|арван долоо|арван найм|арван ес|арав|арван)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) zeroNineteenMap >>= integer
      _ -> Nothing
  }

tensMap :: HashMap Text Integer
tensMap = HashMap.fromList
  [ ( "хорь" , 20 )
  , ( "гуч", 30 )
  , ( "дөч" , 40 )
  , ( "тавь" , 50 )
  , ( "жар" , 60 )
  , ( "дал" , 70 )
  , ( "ная" , 80 )
  , ( "ер" , 90 )
  ]

-- TODO: Single-word composition (#110)
ruleInteger2 :: Rule
ruleInteger2 = Rule
  { name = "integer (20..90)"
  , pattern =
    [ regex "(хорь|гуч|дөч|тавь|жар|дал|ная|ер)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) tensMap >>= integer
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
      (Token Numeral NumeralData{TNumeral.value = v1}:
       _:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v1 + decimalsToDouble v2
      _ -> Nothing
  }

ruleDecimalNumeral :: Rule
ruleDecimalNumeral = Rule
  { name = "decimal number"
  , pattern =
    [ regex "(\\d*\\.\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> parseDecimal True match
      _ -> Nothing
  }

ruleIntegerWithThousandsSeparator :: Rule
ruleIntegerWithThousandsSeparator = Rule
  { name = "integer with thousands separator ."
  , pattern =
    [ regex "(\\d{1,3}(\\.\\d\\d\\d){1,5})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDouble (Text.replace "." Text.empty match) >>= double
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleCouple
  , ruleDecimalNumeral
  , ruleDecimalWithThousandsSeparator
  , ruleFew
  , ruleInteger2
  , ruleInteger3
  , ruleIntegerWithThousandsSeparator
  , ruleIntersect
  , ruleMultiply
  , ruleNumeralDotNumeral
  , ruleNumeralsPrefixWithNegativeOrMinus
  , ruleNumeralsSuffixesKMG
  , ruleNumeralsUnd
  , rulePowersOfTen
  , ruleTen
  , ruleZeroToNineteen
  ]
