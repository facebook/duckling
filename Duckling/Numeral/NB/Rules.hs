-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.NB.Rules
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

ruleIntersectWithAnd :: Rule
ruleIntersectWithAnd = Rule
  { name = "intersect (with and)"
  , pattern =
    [ Predicate hasGrain
    , regex "og"
    , Predicate $ and . sequence [not . isMultipliable, isPositive]
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = val1, TNumeral.grain = Just g}:
       _:
       Token Numeral NumeralData{TNumeral.value = val2}:
       _) | (10 ** fromIntegral g) > val2 -> double $ val1 + val2
      _ -> Nothing
  }

ruleNumeralsPrefixWithNegativeOrMinus :: Rule
ruleNumeralsPrefixWithNegativeOrMinus = Rule
  { name = "numbers prefix with -, negative or minus"
  , pattern =
    [ regex "-|minus|negativ"
    , Predicate isPositive
    ]
  , prod = \case
      (_:Token Numeral nd:_) -> double (TNumeral.value nd * (-1))
      _ -> Nothing
  }

ruleFew :: Rule
ruleFew = Rule
  { name = "few"
  , pattern =
    [ regex "(noen )?få"
    ]
  , prod = \_ -> integer 3
  }

ruleDecimalWithThousandsSeparator :: Rule
ruleDecimalWithThousandsSeparator = Rule
  { name = "decimal with thousands separator"
  , pattern =
    [ regex "(\\d+(\\.\\d\\d\\d)+\\,\\d+)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> let fmt = Text.replace "," "." $ Text.replace "." Text.empty match
        in parseDouble fmt >>= double
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

ruleDecimalNumeral :: Rule
ruleDecimalNumeral = Rule
  { name = "decimal number"
  , pattern =
    [ regex "(\\d*,\\d+)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> parseDecimal False match
      _ -> Nothing
  }

ruleInteger3 :: Rule
ruleInteger3 = Rule
  { name = "integer 21..99"
  , pattern =
    [ oneOf [70, 20, 60, 50, 40, 90, 30, 80]
    , numberBetween 1 10
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v1}:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleSingle :: Rule
ruleSingle = Rule
  { name = "single"
  , pattern =
    [ regex "enkelt"
    ]
  , prod = \_ -> integer 1 >>= withGrain 1
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

ruleNumeralsSuffixesKMG :: Rule
ruleNumeralsSuffixesKMG = Rule
  { name = "numbers suffixes (K, M, G)"
  , pattern =
    [ dimension Numeral
    , regex "([kmg])(?=[\\W\\$€]|$)"
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v}:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
         "k" -> double $ v * 1e3
         "m" -> double $ v * 1e6
         "g" -> double $ v * 1e9
         _   -> Nothing
      _ -> Nothing
  }

rulePowersOfTen :: Rule
rulePowersOfTen = Rule
  { name = "powers of tens"
  , pattern =
    [ regex "(hundre(de)?|tusen?|million(er)?|milliard(?:er)?|billion(?:er)?|billiard(?:er)?|trillion(?:er)?|trilliard(?:er)?|kvadrillion(?:er)?|kvadrilliard(?:er)?)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "hundre"    -> double 1e2 >>= withGrain 2 >>= withMultipliable
        "hundrede"  -> double 1e2 >>= withGrain 2 >>= withMultipliable
        "tuse"      -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "tusen"     -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "million"   -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "millioner" -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "milliard" -> double 1e9 >>= withGrain 9 >>= withMultipliable
        "milliarder" -> double 1e9 >>= withGrain 9 >>= withMultipliable
        "billion" -> double 1e12 >>= withGrain 12 >>= withMultipliable
        "billioner" -> double 1e12 >>= withGrain 12 >>= withMultipliable
        "billiard" -> double 1e15 >>= withGrain 15 >>= withMultipliable
        "billiarder" -> double 1e15 >>= withGrain 15 >>= withMultipliable
        "trillion" -> double 1e18 >>= withGrain 18 >>= withMultipliable
        "trillioner" -> double 1e18 >>= withGrain 18 >>= withMultipliable
        "trilliard" -> double 1e21 >>= withGrain 21 >>= withMultipliable
        "trilliarder" -> double 1e21 >>= withGrain 21 >>= withMultipliable
        "kvadrillion" -> double 1e24 >>= withGrain 24 >>= withMultipliable
        "kvadrillioner" -> double 1e24 >>= withGrain 24 >>= withMultipliable
        "kvadrilliard" -> double 1e27 >>= withGrain 27 >>= withMultipliable
        "kvadrilliarder" -> double 1e27 >>= withGrain 27 >>= withMultipliable
        _           -> Nothing
      _ -> Nothing
  }

ruleAPair :: Rule
ruleAPair = Rule
  { name = "a pair"
  , pattern =
    [ regex "et par"
    ]
  , prod = \_ -> integer 2 >>= withGrain 1
  }

ruleDozen :: Rule
ruleDozen = Rule
  { name = "dozen"
  , pattern =
    [ regex "dusin"
    ]
  , prod = \_ -> integer 12 >>= withGrain 1 >>= withMultipliable
  }

zeroToNineteenMap :: HashMap Text Integer
zeroToNineteenMap = HashMap.fromList
  [ ( "null" , 0 )
  , ( "ingen" , 0 )
  , ( "intet" , 0 )
  , ( "en" , 1 )
  , ( "ett" , 1 )
  , ( "én" , 1 )
  , ( "to" , 2 )
  , ( "tre" , 3 )
  , ( "fire" , 4 )
  , ( "fem" , 5 )
  , ( "seks" , 6 )
  , ( "sju" , 7 )
  , ( "syv" , 7 )
  , ( "åtte" , 8 )
  , ( "ni" , 9 )
  , ( "ti" , 10 )
  , ( "elleve" , 11 )
  , ( "tolv" , 12 )
  , ( "tretten" , 13 )
  , ( "fjorten" , 14 )
  , ( "femten" , 15 )
  , ( "seksten" , 16 )
  , ( "søtten" , 17 )
  , ( "sytten" , 17 )
  , ( "atten" , 18 )
  , ( "nitten" , 19 )
  ]

ruleInteger :: Rule
ruleInteger = Rule
  { name = "integer (0..19)"
  , pattern =
    [ regex "(intet|ingen|null|en|ett|én|to|tretten|tre|fire|femten|fem|seksten|seks|syv|sju|åtte|nitten|ni|ti|elleve|tolv|fjorten|sytten|søtten|atten)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) zeroToNineteenMap >>= integer
      _ -> Nothing
  }

twentyToHundredMap :: HashMap Text Integer
twentyToHundredMap = HashMap.fromList
  [ ("tjueen", 21)
  , ("tjueén", 21)
  , ("tjueto", 22)
  , ("tjuetre", 23)
  , ("tjuefire", 24)
  , ("tjuefem", 25)
  , ("tjueseks", 26)
  , ("tjuesju", 27)
  , ("tjuesyv", 27)
  , ("tjueåtte", 28)
  , ("tjueni", 29)
  , ("trettien", 31)
  , ("trettién", 31)
  , ("trettito", 32)
  , ("trettitre", 33)
  , ("trettifire", 34)
  , ("trettifem", 35)
  , ("trettiseks", 36)
  , ("trettisju", 37)
  , ("trettisyv", 37)
  , ("trettiåtte", 38)
  , ("trettini", 39)
  , ("førtien", 41)
  , ("førtién", 41)
  , ("førtito", 42)
  , ("førtitre", 43)
  , ("førtifire", 44)
  , ("førtifem", 45)
  , ("førtiseks", 46)
  , ("førtisju", 47)
  , ("førtisyv", 47)
  , ("førtiåtte", 48)
  , ("førtini", 49)
  , ("femtien", 51)
  , ("femtién", 51)
  , ("femtito", 52)
  , ("femtitre", 53)
  , ("femtifire", 54)
  , ("femtifem", 55)
  , ("femtiseks", 56)
  , ("femtisju", 57)
  , ("femtisyv", 57)
  , ("femtiåtte", 58)
  , ("femtini", 59)
  , ("sekstien", 61)
  , ("sekstién", 61)
  , ("sekstito", 62)
  , ("sekstitre", 63)
  , ("sekstifire", 64)
  , ("sekstifem", 65)
  , ("sekstiseks", 66)
  , ("sekstisju", 67)
  , ("sekstisyv", 67)
  , ("sekstiåtte", 68)
  , ("sekstini", 69)
  , ("syttien", 71)
  , ("syttién", 71)
  , ("syttito", 72)
  , ("syttitre", 73)
  , ("syttifire", 74)
  , ("syttifem", 75)
  , ("syttiseks", 76)
  , ("syttisju", 77)
  , ("syttisyv", 77)
  , ("syttiåtte", 78)
  , ("syttini", 79)
  , ("søttien", 71)
  , ("søttién", 71)
  , ("søttito", 72)
  , ("søttitre", 73)
  , ("søttifire", 74)
  , ("søttifem", 75)
  , ("søttiseks", 76)
  , ("søttisju", 77)
  , ("søttisyv", 77)
  , ("søttiåtte", 78)
  , ("søttini", 79)
  , ("åttien", 81)
  , ("åttién", 81)
  , ("åttito", 82)
  , ("åttitre", 83)
  , ("åttifire", 84)
  , ("åttifem", 85)
  , ("åttiseks", 86)
  , ("åttisju", 87)
  , ("åttisyv", 87)
  , ("åttiåtte", 88)
  , ("åttini", 89)
  , ("nittien", 91)
  , ("nittién", 91)
  , ("nittito", 92)
  , ("nittitre", 93)
  , ("nittifire", 94)
  , ("nittifem", 95)
  , ("nittiseks", 96)
  , ("nittisju", 97)
  , ("nittisyv", 97)
  , ("nittiåtte", 98)
  , ("nittini", 99)
  ]

ruleInteger4 :: Rule
ruleInteger4 = Rule
  { name = "integer (21..99)"
  , pattern =
    [ regex "(tjueen|tjueén|tjueto|tjuetre|tjuefire|tjuefem|tjueseks|tjuesju|tjuesyv|tjueåtte|tjueni\
            \|trettien|trettién|trettito|trettitre|trettifire|trettifem|trettiseks|trettisju|trettisyv|trettiåtte|trettini\
            \|førtien|førtién|førtito|førtitre|førtifire|førtifem|førtiseks|førtisju|førtisyv|førtiåtte|førtini\
            \|femtien|femtién|femtito|femtitre|femtifire|femtifem|femtiseks|femtisju|femtisyv|femtiåtte|femtini\
            \|sekstien|sekstién|sekstito|sekstitre|sekstifire|sekstifem|sekstiseks|sekstisju|sekstisyv|sekstiåtte|sekstini\
            \|syttien|syttién|syttito|syttitre|syttifire|syttifem|syttiseks|syttisju|syttisyv|syttiåtte|syttini\
            \|søttien|søttién|søttito|søttitre|søttifire|søttifem|søttiseks|søttisju|søttisyv|søttiåtte|søttini\
            \|åttien|åttién|åttito|åttitre|åttifire|åttifem|åttiseks|åttisju|åttisyv|åttiåtte|åttini\
            \|nittien|nittién|nittito|nittitre|nittifire|nittifem|nittiseks|nittisju|nittisyv|nittiåtte|nittini\
            \)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) twentyToHundredMap >>= integer
      _ -> Nothing
  }


tensMap :: HashMap Text Integer
tensMap = HashMap.fromList
  [ ( "tyve" , 20 )
  , ( "tjue" , 20 )
  , ( "tredve" , 30 )
  , ( "tretti" , 30 )
  , ( "førti" , 40 )
  , ( "femti" , 50 )
  , ( "seksti" , 60 )
  , ( "sytti" , 70 )
  , ( "søtti" , 70 )
  , ( "åtti" , 80 )
  , ( "nitti" , 90 )
  ]

ruleInteger2 :: Rule
ruleInteger2 = Rule
  { name = "integer (20..90)"
  , pattern =
    [ regex "(tyve|tjue|tredve|tretti|førti|femti|seksti|sytti|søtti|åtti|nitti)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) tensMap >>= integer
      _ -> Nothing
  }

ruleNumeralDotNumeral :: Rule
ruleNumeralDotNumeral = Rule
  { name = "number dot number"
  , pattern =
    [ dimension Numeral
    , regex "komma"
    , Predicate $ not . hasGrain
    ]
  , prod = \case
      (Token Numeral nd1:_:Token Numeral nd2:_) ->
        double $ TNumeral.value nd1 + decimalsToDouble (TNumeral.value nd2)
      _ -> Nothing
  }

ruleIntegerWithThousandsSeparator :: Rule
ruleIntegerWithThousandsSeparator = Rule
  { name = "integer with thousands separator ."
  , pattern =
    [ regex "(\\d{1,3}((?:\\.| )\\d\\d\\d){1,5})"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) -> parseDouble
        (Text.replace " " Text.empty $ Text.replace "." Text.empty $ match)
        >>= double
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleAPair
  , ruleDecimalNumeral
  , ruleDecimalWithThousandsSeparator
  , ruleDozen
  , ruleFew
  , ruleInteger
  , ruleInteger2
  , ruleInteger3
  , ruleInteger4
  , ruleIntegerWithThousandsSeparator
  , ruleIntersect
  , ruleIntersectWithAnd
  , ruleMultiply
  , ruleNumeralDotNumeral
  , ruleNumeralsPrefixWithNegativeOrMinus
  , ruleNumeralsSuffixesKMG
  , rulePowersOfTen
  , ruleSingle
  ]
