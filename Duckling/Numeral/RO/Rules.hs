-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.RO.Rules
  ( rules ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import qualified Duckling.Numeral.Types as TNumeral
import Duckling.Regex.Types
import Duckling.Types

ruleNumeralsPrefixWithOrMinus :: Rule
ruleNumeralsPrefixWithOrMinus = Rule
  { name = "numbers prefix with - or minus"
  , pattern =
    [ regex "-|minus\\s?"
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral nd:_) -> double (TNumeral.value nd * (-1))
      _ -> Nothing
  }

ruleIntegerNumeric :: Rule
ruleIntegerNumeric = Rule
  { name = "integer (numeric)"
  , pattern =
    [ regex "(\\d{1,18})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        v <- toInteger <$> parseInt match
        integer v
      _ -> Nothing
  }

ruleSpecialCompositionForMissingHundredsLikeInOneTwentyTwo :: Rule
ruleSpecialCompositionForMissingHundredsLikeInOneTwentyTwo = Rule
  { name = "special composition for missing hundreds like in one twenty two"
  , pattern =
    [ numberBetween 1 10
    , numberBetween 11 100
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = hundreds}):
       Token Numeral (NumeralData {TNumeral.value = rest}):
       _) -> double $ hundreds * 100 + rest
      _ -> Nothing
  }

ruleDecimalWithThousandsSeparator :: Rule
ruleDecimalWithThousandsSeparator = Rule
  { name = "decimal with thousands separator"
  , pattern =
    [ regex "(\\d+(\\.\\d\\d\\d)+,\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> let dot = Text.singleton '.'
                 comma = Text.singleton ','
                 fmt = Text.replace comma dot $ Text.replace dot Text.empty match
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

ruleInteger3 :: Rule
ruleInteger3 = Rule
  { name = "integer 21..99"
  , pattern =
    [ oneOf [20, 30 .. 90]
    , numberBetween 1 10
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v1}):
       Token Numeral (NumeralData {TNumeral.value = v2}):
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleMultiply :: Rule
ruleMultiply = Rule
  { name = "compose by multiplication"
  , pattern =
    [ dimension Numeral
    , numberWith TNumeral.multipliable id
    ]
  , prod = \tokens -> case tokens of
      (token1:token2:_) -> multiply token1 token2
      _ -> Nothing
  }

ruleIntersect :: Rule
ruleIntersect = Rule
  { name = "intersect"
  , pattern =
    [ numberWith (fromMaybe 0 . TNumeral.grain) (>1)
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = val1, TNumeral.grain = Just g}):
       Token Numeral (NumeralData {TNumeral.value = val2}):
       _) | (10 ** fromIntegral g) > val2 -> double $ val1 + val2
      _ -> Nothing
  }

ruleIntersectCuI :: Rule
ruleIntersectCuI = Rule
  { name = "intersect (cu și)"
  , pattern =
    [ numberWith (fromMaybe 0 . TNumeral.grain) (>1)
    , regex "(s|\x0219)i"
    , numberWith TNumeral.multipliable not
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = val1, TNumeral.grain = Just g}):
       _:
       Token Numeral (NumeralData {TNumeral.value = val2}):
       _) | (10 ** fromIntegral g) > val2 -> double $ val1 + val2
      _ -> Nothing
  }

ruleNumeralsSuffixesWithNegativ :: Rule
ruleNumeralsSuffixesWithNegativ = Rule
  { name = "numbers suffixes with (negativ)"
  , pattern =
    [ dimension Numeral
    , regex "(negativ|neg)"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v}):
       _) -> double $ v * (-1)
      _ -> Nothing
  }

rulePowersOfTen :: Rule
rulePowersOfTen = Rule
  { name = "powers of tens"
  , pattern =
    [ regex "(sut(a|e|\x0103)?|milio(n|ane)?|miliar(de?)?|mi[ei]?)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "suta"      -> double 1e2 >>= withGrain 2 >>= withMultipliable
        "sute"      -> double 1e2 >>= withGrain 2 >>= withMultipliable
        "sut\x0103" -> double 1e2 >>= withGrain 2 >>= withMultipliable
        "mi"        -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "mie"       -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "mii"       -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "milio"     -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "milion"    -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "milioane"  -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "miliar"    -> double 1e9 >>= withGrain 9 >>= withMultipliable
        "miliard"   -> double 1e9 >>= withGrain 9 >>= withMultipliable
        "miliarde"  -> double 1e9 >>= withGrain 9 >>= withMultipliable
        _           -> Nothing
      _ -> Nothing
  }

zeroTenMap :: HashMap Text Integer
zeroTenMap = HashMap.fromList
  [ ("zero", 0)
  , ("nimic", 0)
  , ("nicio", 0)
  , ("nici o", 0)
  , ("nici una", 0)
  , ("nici unu", 0)
  , ("nici unul", 0)
  , ("un", 1)
  , ("una", 1)
  , ("unu", 1)
  , ("unul", 1)
  , ("intai", 1)
  , ("\x00eentai", 1)
  , ("int\x00e2i", 1)
  , ("\x00eent\x00e2i", 1)
  , ("o", 1)
  , ("doi", 2)
  , ("doua", 2)
  , ("dou\x0103", 2)
  , ("trei", 3)
  , ("patru", 4)
  , ("cinci", 5)
  , ("sase", 6)
  , ("\537ase", 6)
  , ("sapte", 7)
  , ("\537apte", 7)
  , ("opt", 8)
  , ("noua", 9)
  , ("nou\x0103", 9)
  , ("zece", 10)
  , ("zeci", 10)
  ]

ruleIntegerZeroTen :: Rule
ruleIntegerZeroTen = Rule
  { name = "integer (0..10)"
  , pattern =
    [ regex "(zero|nimic|nici(\\s?o|\\sun(a|ul?))|una|unul?|doi|dou(a|\x0103)|trei|patru|cinci|(s|\x0219)ase|(s|\x0219)apte|opt|nou(a|\x0103)|zec[ei]|(i|\x00ee)nt(a|\x00e2)i|un|o)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) zeroTenMap >>= integer
      _ -> Nothing
  }

elevenNineteenMap :: HashMap Text Integer
elevenNineteenMap = HashMap.fromList
  [ ("un", 11)
  , ("doi", 12)
  , ("trei", 13)
  , ("pai", 14)
  , ("cin", 15)
  , ("cinci", 15)
  , ("sai", 16)
  , ("\537ai", 16)
  , ("sapti", 17)
  , ("\537apti", 17)
  , ("sapte", 17)
  , ("\537apte", 17)
  , ("opti", 18)
  , ("opt", 18)
  , ("noua", 19)
  , ("nou\x0103", 19)
  ]

ruleInteger :: Rule
ruleInteger = Rule
  { name = "integer (11..19)"
  , pattern =
    [ regex "(cin|sapti|opti)(s|\x0219)pe|(cinci|(s|\x0219)apte|opt)sprezece|(un|doi|trei|pai|(s|\x0219)ai|nou(a|\x0103))((s|\x0219)pe|sprezece)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (e1:_:e2:_:r:_)):_) -> do
        match <- case () of
          _ | not $ Text.null e1 -> Just e1
            | not $ Text.null e2 -> Just e2
            | not $ Text.null r  -> Just r
            | otherwise          -> Nothing
        HashMap.lookup (Text.toLower match) elevenNineteenMap >>= integer
      _ -> Nothing
  }

ruleInteger2 :: Rule
ruleInteger2 = Rule
  { name = "integer (20..90)"
  , pattern =
    [ regex "(dou(a|\x0103)|trei|patru|cinci|(s|\x0219)ai|(s|\x0219)apte|opt|nou(a|\x0103))\\s?zeci"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        unit <- HashMap.lookup (Text.toLower match) zeroTenMap
        integer (unit * 10) >>= withGrain 2 >>= withMultipliable
      _ -> Nothing
  }

ruleIntegerCuSeparatorDeMiiDot :: Rule
ruleIntegerCuSeparatorDeMiiDot = Rule
  { name = "integer cu separator de mii dot"
  , pattern =
    [ regex "(\\d{1,3}(\\.\\d\\d\\d){1,5})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> let fmt = Text.replace (Text.singleton '.') Text.empty match
        in parseDouble fmt >>= double
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleDecimalNumeral
  , ruleDecimalWithThousandsSeparator
  , ruleInteger
  , ruleIntegerZeroTen
  , ruleInteger2
  , ruleInteger3
  , ruleIntegerCuSeparatorDeMiiDot
  , ruleIntegerNumeric
  , ruleIntersect
  , ruleIntersectCuI
  , ruleMultiply
  , ruleNumeralsPrefixWithOrMinus
  , ruleNumeralsSuffixesWithNegativ
  , rulePowersOfTen
  , ruleSpecialCompositionForMissingHundredsLikeInOneTwentyTwo
  ]
