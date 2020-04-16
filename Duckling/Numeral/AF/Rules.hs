-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Numeral.AF.Rules
  ( rules
  ) where

import Control.Applicative ((<|>))
import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Numeral.Types (NumeralData(..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral

zeroAndTenMap :: HashMap Text Integer
zeroAndTenMap = HashMap.fromList
  [ ( "nul" , 0  )
  , ( "geen", 0  )
  , ( "niks", 0  )
  , ( "zero", 0  )
  , ( "tien", 10 )
  ]

oneToNineMap :: HashMap Text Integer
oneToNineMap = HashMap.fromList
  [ ( "een" , 1 )
  , ( "twee", 2 )
  , ( "drie", 3 )
  , ( "vier", 4 )
  , ( "vyf" , 5 )
  , ( "ses" , 6 )
  , ( "sewe", 7 )
  , ( "agt" , 8 )
  , ( "ag"  , 8 )
  , ( "nege", 9 )
  ]

ruleNumeral :: Rule
ruleNumeral = Rule
  { name = "number (0..10)"
  , pattern =
    [ regex . Text.unpack $
        Text.concat [ "(", Text.intercalate "|"
          ((HashMap.keys zeroAndTenMap) ++ (HashMap.keys oneToNineMap)), ")" ]
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        let x = Text.toLower match in
        (HashMap.lookup x oneToNineMap >>= integer) <|>
        (HashMap.lookup x zeroAndTenMap >>= integer)
      _ -> Nothing
  }

elevenToNineteenMap :: HashMap Text Integer
elevenToNineteenMap = HashMap.fromList
  [ ( "elf"      , 11 )
  , ( "twaalf"   , 12 )
  , ( "dertien"  , 13 )
  , ( "veertien" , 14 )
  , ( "vyftien"  , 15 )
  , ( "sestien"  , 16 )
  , ( "sewentien", 17 )
  , ( "agtien"   , 18 )
  , ( "negentien", 19 )
  , ( "neentien" , 19 )
  ]

ruleElevenToNineteen :: Rule
ruleElevenToNineteen = Rule
  { name = "number (11..19)"
  , pattern =
    [ regex "(elf|twaalf|dertien|veertien|vyftien|sestien|sewentien|agtien|neg?entien)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) elevenToNineteenMap >>= integer
      _ -> Nothing
  }

tensMap :: HashMap Text Integer
tensMap = HashMap.fromList
  [ ( "twin" , 20 )
  , ( "der"  , 30 )
  , ( "veer" , 40 )
  , ( "vyf"  , 50 )
  , ( "ses"  , 60 )
  , ( "sewen", 70 )
  , ( "tag"  , 80 )
  , ( "negen", 90 )
  , ( "neen" , 90 )
  ]

ruleTens :: Rule
ruleTens = Rule
  { name = "integer (20,30..90)"
  , pattern =
    [ regex "(twin|der|veer|vyf|ses|sewen|tag|neg?en)tig"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) tensMap >>= integer
      _ -> Nothing
  }

ruleCompositeTens :: Rule
ruleCompositeTens = Rule
  { name = "integer ([3-9][1-9])"
  , pattern =
    [ numberBetween 1 10
    , regex "en"
    , oneOf [20, 30..90]
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = units}:
       _:
       Token Numeral NumeralData{TNumeral.value = tens}:
       _) -> double $ tens + units
      _ -> Nothing
  }

ruleDecimals :: Rule
ruleDecimals = Rule
  { name = "decimal number"
  , pattern =
    [ regex "(\\d*\\,\\d+)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) -> parseDecimal False match
      _ -> Nothing
  }

rulePowersOfTen :: Rule
rulePowersOfTen = Rule
  { name = "powers of tens"
  , pattern =
    [ regex "(honderd|duisend)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "honderd" -> double 1e2 >>= withGrain 2 >>= withMultipliable
        "duisend" -> double 1e3 >>= withGrain 3 >>= withMultipliable
        _ -> Nothing
      _ -> Nothing
  }

ruleDozen :: Rule
ruleDozen = Rule
  { name = "a dozen of"
  , pattern =
    [ regex "dosyn"
    ]
  , prod = \_ -> integer 12 >>= withMultipliable >>= notOkForAnyTime
  }

ruleSum :: Rule
ruleSum = Rule
  { name = "intersect 2 numbers"
  , pattern =
    [ Predicate $ and . sequence [hasGrain, isPositive]
    , Predicate $ and . sequence [not . isMultipliable, isPositive]
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = val1, TNumeral.grain = Just g}:
       Token Numeral NumeralData{TNumeral.value = val2}:
       _) | (10 ** fromIntegral g) > val2 -> double $ val1 + val2
      _ -> Nothing
  }

ruleMultiply :: Rule
ruleMultiply = Rule
  { name = "compose by multiplication"
  , pattern =
    [ dimension Numeral
    , Predicate isMultipliable
    ]
  , prod = \case
      (token1:token2:_) -> multiply token1 token2
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleNumeral
  , ruleDozen
  , ruleDecimals
  , rulePowersOfTen
  , ruleElevenToNineteen
  , ruleTens
  , ruleCompositeTens
  , ruleSum
  , ruleMultiply
  ]
