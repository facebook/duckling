-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.RO.Rules
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
import Duckling.Numeral.Types (NumeralData(..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral

ruleNumeralsPrefixWithOrMinus :: Rule
ruleNumeralsPrefixWithOrMinus = Rule
  { name = "numbers prefix with - or minus"
  , pattern =
    [ regex "-|minus"
    , Predicate isPositive
    ]
  , prod = \case
      (_:Token Numeral nd:_) -> double (TNumeral.value nd * (-1))
      _ -> Nothing
  }

ruleDecimalWithThousandsSeparator :: Rule
ruleDecimalWithThousandsSeparator = Rule
  { name = "decimal with thousands separator"
  , pattern =
    [ regex "(\\d+(\\.\\d\\d\\d)+,\\d+)"
    ]
  , prod = \case
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
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) -> parseDecimal False match
      _ -> Nothing
  }

ruleInteger3 :: Rule
ruleInteger3 = Rule
  { name = "integer 21..99"
  , pattern =
    [ oneOf [20, 30 .. 90]
    , numberBetween 1 10
    ]
  , prod = \case
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
  , prod = \case
      (token1:token2:_) -> multiply token1 token2
      _ -> Nothing
  }

ruleMultiplyDe :: Rule
ruleMultiplyDe = Rule
  { name = "compose by multiplication"
  , pattern =
    [ numberWith TNumeral.value (>= 20)
    , regex "de"
    , Predicate isMultipliable
    ]
  , prod = \case
      (token1:_:token2:_) -> multiply token1 token2
      _ -> Nothing
  }

ruleIntersect :: Rule
ruleIntersect = Rule
  { name = "intersect"
  , pattern =
    [ Predicate hasGrain
    , Predicate $ and . sequence [not . isMultipliable, isPositive]
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = val1, TNumeral.grain = Just g}:
       Token Numeral NumeralData{TNumeral.value = val2}:
       _) | (10 ** fromIntegral g) > val2 -> double $ val1 + val2
      _ -> Nothing
  }

ruleIntersectCuI :: Rule
ruleIntersectCuI = Rule
  { name = "intersect (cu și)"
  , pattern =
    [ Predicate hasGrain
    , regex "[sș]i"
    , Predicate $ and . sequence [not . isMultipliable, isPositive]
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = val1, TNumeral.grain = Just g}:
       _:
       Token Numeral NumeralData{TNumeral.value = val2}:
       _) | (10 ** fromIntegral g) > val2 -> double $ val1 + val2
      _ -> Nothing
  }

ruleNumeralsSuffixesWithNegativ :: Rule
ruleNumeralsSuffixesWithNegativ = Rule
  { name = "numbers suffixes with (negativ)"
  , pattern =
    [ Predicate isPositive
    , regex "neg(ativ)?"
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v}:
       _) -> double $ v * (-1)
      _ -> Nothing
  }

rulePowersOfTen :: Rule
rulePowersOfTen = Rule
  { name = "powers of tens"
  , pattern =
    [ regex "(sut(a|e|ă)?|milio(n|ane)?|miliar(de?)?|mi[ei]?)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "suta"      -> double 1e2 >>= withGrain 2 >>= withMultipliable
        "sute"      -> double 1e2 >>= withGrain 2 >>= withMultipliable
        "sută" -> double 1e2 >>= withGrain 2 >>= withMultipliable
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
  , ("întai", 1)
  , ("intâi", 1)
  , ("întâi", 1)
  , ("o", 1)
  , ("doi", 2)
  , ("doua", 2)
  , ("două", 2)
  , ("trei", 3)
  , ("patru", 4)
  , ("cinci", 5)
  , ("sase", 6)
  , ("\537ase", 6)
  , ("sapte", 7)
  , ("\537apte", 7)
  , ("opt", 8)
  , ("noua", 9)
  , ("nouă", 9)
  , ("zece", 10)
  , ("zeci", 10)
  ]

ruleIntegerZeroTen :: Rule
ruleIntegerZeroTen = Rule
  { name = "integer (0..10)"
  , pattern =
    [ regex "(zero|nimic|nici(\\s?o|\\sun(a|ul?))|una|unul?|doi|dou(a|ă)|trei|patru|cinci|(s|ș)ase|(s|ș)apte|opt|nou(a|ă)|zec[ei]|(i|î)nt(a|â)i|un|o)"
    ]
  , prod = \case
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
  , ("nouă", 19)
  ]

ruleInteger :: Rule
ruleInteger = Rule
  { name = "integer (11..19)"
  , pattern =
    [ regex "(cin|sapti|opti)(s|ș)pe|(cinci|(s|ș)apte|opt)sprezece|(un|doi|trei|pai|(s|ș)ai|nou(a|ă))((s|ș)pe|sprezece)"
    ]
  , prod = \case
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
    [ regex "(dou[aă]|trei|patru|cinci|[sș]ai|[sș]apte|opt|nou[aă])\\s?zeci"
    ]
  , prod = \case
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
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> let fmt = Text.replace "." Text.empty match
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
  , ruleIntersect
  , ruleIntersectCuI
  , ruleMultiply
  , ruleMultiplyDe
  , ruleNumeralsPrefixWithOrMinus
  , ruleNumeralsSuffixesWithNegativ
  , rulePowersOfTen
  ]
