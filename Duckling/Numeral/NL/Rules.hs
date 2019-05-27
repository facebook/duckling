-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.NL.Rules
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

ruleNumeralsPrefixWithNegativeOrMinus :: Rule
ruleNumeralsPrefixWithNegativeOrMinus = Rule
  { name = "numbers prefix with -, negative or minus"
  , pattern =
    [ regex "-|min|minus|negatief"
    , Predicate isPositive
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral nd:_) -> double (TNumeral.value nd * (-1))
      _ -> Nothing
  }

ruleFew :: Rule
ruleFew = Rule
  { name = "few"
  , pattern =
    [ regex "meerdere"
    ]
  , prod = \_ -> integer 3
  }

ruleTen :: Rule
ruleTen = Rule
  { name = "ten"
  , pattern =
    [ regex "tien"
    ]
  , prod = \_ -> integer 10 >>= withGrain 1
  }

ruleDecimalWithThousandsSeparator :: Rule
ruleDecimalWithThousandsSeparator = Rule
  { name = "decimal with thousands separator"
  , pattern =
    [ regex "(\\d+(\\.\\d\\d\\d)+,\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> let fmt = Text.replace "," "." $ Text.replace "." Text.empty match
        in parseDouble fmt >>= double
      _ -> Nothing
  }

ruleDecimalNumeral :: Rule
ruleDecimalNumeral = Rule
  { name = "decimal number"
  , pattern =
    [ regex "(\\d*,\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> parseDecimal False match
      _ -> Nothing
  }

-- TODO: Single-word composition (#110)
ruleInteger3 :: Rule
ruleInteger3 = Rule
  { name = "integer ([2-9][1-9])"
  , pattern =
    [ regex "(een|twee|drie|vier|vijf|zes|zeven|acht|negen)(?:e|ë)n(twintig|dertig|veertig|vijftig|zestig|zeventig|tachtig|negentig)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        v1 <- HashMap.lookup (Text.toLower m1) zeroNineteenMap
        v2 <- HashMap.lookup (Text.toLower m2) dozenMap
        integer $ v1 + v2
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

ruleNumeralsEn :: Rule
ruleNumeralsEn = Rule
  { name = "numbers en"
  , pattern =
    [ numberBetween 1 10
    , regex "-?en-?"
    , oneOf [20, 30 .. 90]
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v1}:
       _:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v1 + v2
      _ -> Nothing
  }

rulePowersOfTen :: Rule
rulePowersOfTen = Rule
  { name = "powers of tens"
  , pattern =
    [ regex "(honderd|duizend|miljoen)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "honderd" -> double 1e2 >>= withGrain 2 >>= withMultipliable
        "duizend" -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "miljoen" -> double 1e6 >>= withGrain 6 >>= withMultipliable
        _         -> Nothing
      _ -> Nothing
  }

ruleCouple :: Rule
ruleCouple = Rule
  { name = "couple"
  , pattern =
    [ regex "(een )?paar"
    ]
  , prod = \_ -> integer 2
  }

ruleDozen :: Rule
ruleDozen = Rule
  { name = "dozen"
  , pattern =
    [ regex "dozijn"
    ]
  , prod = \_ -> integer 12 >>= withGrain 1
  }

ruleGross :: Rule
ruleGross = Rule
  { name = "gros"
  , pattern =
    [ regex "gros"
    ]
  , prod = \_ -> integer 144 >>= withGrain 1
  }

zeroNineteenMap :: HashMap Text Integer
zeroNineteenMap = HashMap.fromList
  [ ("niks", 0)
  , ("nul", 0)
  , ("geen", 0)
  , ("één", 1)
  , ("een", 1)
  , ("twee", 2)
  , ("drie", 3)
  , ("vier", 4)
  , ("vijf", 5)
  , ("zes", 6)
  , ("zeven", 7)
  , ("acht", 8)
  , ("negen", 9)
  , ("tien", 10)
  , ("elf", 11)
  , ("twaalf", 12)
  , ("dertien", 13)
  , ("veertien", 14)
  , ("vijftien", 15)
  , ("zestien", 16)
  , ("zeventien", 17)
  , ("achtien", 18)
  , ("negentien", 19)
  ]

ruleInteger :: Rule
ruleInteger = Rule
  { name = "integer (0..19)"
  , pattern =
    [ regex "(geen|nul|niks|een|één|twee|drie|vier|vijftien|vijf|zestien|zes|zeventien|zeven|achtien|acht|negentien|negen|tien|elf|twaalf|dertien|veertien)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) zeroNineteenMap >>= integer
      _ -> Nothing
  }

dozenMap :: HashMap Text Integer
dozenMap = HashMap.fromList
  [ ("twintig", 20)
  , ("dertig", 30)
  , ("veertig", 40)
  , ("vijftig", 50)
  , ("zestig", 60)
  , ("zeventig", 70)
  , ("tachtig", 80)
  , ("negentig", 90)
  ]

ruleInteger2 :: Rule
ruleInteger2 = Rule
  { name = "integer (20..90)"
  , pattern =
    [ regex "(twintig|dertig|veertig|vijftig|zestig|zeventig|tachtig|negentig)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) dozenMap >>= integer
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleCouple
  , ruleDecimalNumeral
  , ruleDecimalWithThousandsSeparator
  , ruleDozen
  , ruleGross
  , ruleFew
  , ruleInteger
  , ruleInteger2
  , ruleInteger3
  , ruleIntersect
  , ruleMultiply
  , ruleNumeralsEn
  , ruleNumeralsPrefixWithNegativeOrMinus
  , ruleNumeralsSuffixesKMG
  , rulePowersOfTen
  , ruleTen
  ]
