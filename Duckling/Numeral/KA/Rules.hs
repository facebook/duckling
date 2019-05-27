-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.KA.Rules
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
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral

zeroNineteenMap :: HashMap Text Integer
zeroNineteenMap = HashMap.fromList
  [ ( "ნოლ"   , 0 )
  , ( "ნულ"      , 0 )
  , ( "ნული"      , 0 )
  , ( "ნოლ"   , 0 )
  , ( "ნოლი"   , 0 )
  , ( "ერთი"      , 1 )
  , ( "ორი"      , 2 )
  , ( "ორ"      , 2 )
  , ( "სამი"    , 3 )
  , ( "სამ"    , 3 )
  , ( "ოთხი"     , 4 )
  , ( "ოთხ"     , 4 )
  , ( "ხუთი"     , 5 )
  , ( "ხუთ"     , 5 )
  , ( "ექვსი"      , 6 )
  , ( "ექვს"      , 6 )
  , ( "შვიდი"    , 7 )
  , ( "შვიდ"    , 7 )
  , ( "რვა"    , 8 )
  , ( "რვ"    , 8 )
  , ( "ცხრა"     , 9 )
  , ( "ცხრ"     , 9 )
  , ( "ათი"      , 10 )
  , ( "აათი"      , 10 )
  , ( "თერთმეტი"   , 11 )
  , ( "თერთმეტ"   , 11 )
  , ( "თორმეტი"   , 12 )
  , ( "თორმეტ"   , 12 )
  , ( "ცამეტი" , 13 )
  , ( "ცამეტ" , 13 )
  , ( "თოთხმეტი" , 14 )
  , ( "თოთხმეტ" , 14 )
  , ( "თხუთმეტი"  , 15 )
  , ( "თხუთმეტ"  , 15 )
  , ( "თექვსმეტი"  , 16 )
  , ( "თექვსმეტ"  , 16 )
  , ( "ჩვიდმეტი", 17 )
  , ( "ჩვიდმეტ", 17 )
  , ( "თვრამეტი" , 18 )
  , ( "თვრამეტ" , 18 )
  , ( "ცხრამეტი" , 19 )
  , ( "ცხრამეტ" , 19 )
  ]

informalMap :: HashMap Text Integer
informalMap = HashMap.fromList
  [ ( "ერთი"     , 1 )
  , ( "წყვილი"   , 2 )
  , ( "წყვილები"     , 2 )
  , ( "ცოტა"      , 3 )
  , ( "რამდენიმე"        , 3 )
  , ( "რამოდენიმე"        , 3 )
  ]

ruleToNineteen :: Rule
ruleToNineteen = Rule
  { name = "integer (0..19)"
  , pattern =
    [ regex "(წყვილ(ებ)?ი|ცოტა|რამდენიმე|რამოდენიმე|ნოლი?|ნული?|ერთი|ორი?|სამი?|ოთხი?|ხუთი?|ექვსი?|შვიდი?|რვა|თერთმეტი?|თორმეტი?|ცამეტი?|თოთხმეტი?|თხუთმეტი?|თექვსმეტი?|ჩვიდმეტი?|თვრამეტი?|ცხრამეტი?|ცხრა|ა?ათი)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        let x = Text.toLower match in
        (HashMap.lookup x zeroNineteenMap >>= integer) <|>
        (HashMap.lookup x informalMap >>= integer >>= notOkForAnyTime)
      _ -> Nothing
  }

tensMap :: HashMap Text Integer
tensMap = HashMap.fromList
  [ ( "ოცი"  , 20 )
  , ( "ოცდა"  , 20 )
  , ( "ოც"  , 20 )
  , ( "ოცდაათ"  , 30 )
  , ( "ოცდაათი"  , 30 )
  , ( "ორმოც"   , 40 )
  , ( "ორმოცი"   , 40 )
  , ( "ორმოცდა"   , 40 )
  , ( "ორმოცდაათ"   , 50 )
  , ( "ორმოცდაათი"   , 50 )
  , ( "სამოც"   , 60 )
  , ( "სამოცი"   , 60 )
  , ( "სამოცდა"   , 60 )
  , ( "სამოცდაათ" , 70 )
  , ( "სამოცდაათი" , 70 )
  , ( "ოთხმოც"  , 80 )
  , ( "ოთხმოცი"  , 80 )
  , ( "ოთხმოცდა"  , 80 )
  , ( "ოთხმოცდაათ"  , 90 )
  , ( "ოთხმოცდაათი"  , 90 )
  ]

ruleTens :: Rule
ruleTens = Rule
  { name = "integer (20..90)"
  , pattern =
    [ regex "(ოცდაათი?|ორმოცდაათი?|სამოცდაათი?|ოთხმოცდაათი?|ოცდა|ორმოცდა|სამოცდა|ოთხმოცდა|ოცი?|ორმოცი?|სამოცი?|ოთხმოცი?)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) tensMap >>= integer
      _ -> Nothing
  }

hundredsMap :: HashMap Text Integer
hundredsMap = HashMap.fromList
  [ ( "ასი"  , 100 )
  , ( "ორასი"  , 200 )
  , ( "სამასი"  , 300 )
  , ( "ოთხასი"  , 400 )
  , ( "ხუთასი"  , 500 )
  , ( "ექვსასი"  , 600 )
  , ( "შვიდასი"  , 700 )
  , ( "რვაასი"  , 800 )
  , ( "ცხრაასი"   , 900 )
  , ( "ას"  , 100 )
  , ( "ორას"  , 200 )
  , ( "სამას"  , 300 )
  , ( "ოთხას"  , 400 )
  , ( "ხუთას"  , 500 )
  , ( "ექვსას"  , 600 )
  , ( "შვიდას"  , 700 )
  , ( "რვაას"  , 800 )
  , ( "ცხრაას"   , 900 )
  , ( "ორ ას"  , 200 )
  , ( "სამ ას"  , 300 )
  , ( "ოთხ ას"  , 400 )
  , ( "ხუთ ას"  , 500 )
  , ( "ექვს ას"  , 600 )
  , ( "შვიდ ას"  , 700 )
  , ( "რვა ას"  , 800 )
  , ( "ცხრა ას"   , 900 )
  , ( "ორ ასი"  , 200 )
  , ( "სამ ასი"  , 300 )
  , ( "ოთხ ასი"  , 400 )
  , ( "ხუთ ასი"  , 500 )
  , ( "ექვს ასი"  , 600 )
  , ( "შვიდ ასი"  , 700 )
  , ( "რვა ასი"  , 800 )
  , ( "ცხრა ასი"   , 900 )
  ]

ruleHundreds :: Rule
ruleHundreds = Rule
  { name = "integer (800..900)"
  , pattern =
    [ regex "(ასი?|ორ ?ასი?|სამ ?ასი?|ოთხ ?ასი?|ხუთ ?ასი?|ექვს ?ასი?|შვიდ ?ასი?|რვა ?ასი?|ცხრა ?ასი?)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) hundredsMap >>= integer
      _ -> Nothing
  }

rulePowersOfTen :: Rule
rulePowersOfTen = Rule
  { name = "powers of tens"
  , pattern =
    [ regex "(ათასი?|მილიონი?|მილიარდი?)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "ათასი" -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "ათას" -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "მილიონი"  -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "მილიონ"  -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "მილიარდი"  -> double 1e9 >>= withGrain 9 >>= withMultipliable
        "მილიარდ"  -> double 1e9 >>= withGrain 9 >>= withMultipliable
        _          -> Nothing
      _ -> Nothing
  }

ruleCompositeTens :: Rule
ruleCompositeTens = Rule
  { name = "integer 21..99"
  , pattern =
    [ oneOf [20,40..90]
    , numberBetween 1 20
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = tens}:
       Token Numeral NumeralData{TNumeral.value = units}:
       _) -> double $ tens + units
      _ -> Nothing
  }

ruleCompositeHundreds :: Rule
ruleCompositeHundreds = Rule
  { name = "integer 100..999"
  , pattern =
    [ oneOf [100,200..900]
    , oneOf [20,40..90]
    , numberBetween 1 20
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = hundreds}:
       Token Numeral NumeralData{TNumeral.value = tens}:
       Token Numeral NumeralData{TNumeral.value = units}:
       _) -> double $ hundreds + tens + units
      _ -> Nothing
  }

ruleCompositeHundredsAndUnits :: Rule
ruleCompositeHundredsAndUnits = Rule
  { name = "integer 100..999"
  , pattern =
    [ oneOf [100,200..900]
    , numberBetween 1 20
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = hundreds}:
       Token Numeral NumeralData{TNumeral.value = units}:
       _) -> double $ hundreds + units
      _ -> Nothing
  }

ruleCompositeHundredsAndTens :: Rule
ruleCompositeHundredsAndTens = Rule
  { name = "integer 100..999"
  , pattern =
    [ oneOf [100,200..900]
    , oneOf [10,20..90]
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = hundreds}:
       Token Numeral NumeralData{TNumeral.value = tens}:
       _) -> double $ hundreds + tens
      _ -> Nothing
  }

ruleDotSpelledOut :: Rule
ruleDotSpelledOut = Rule
  { name = "one point 2"
  , pattern =
    [ dimension Numeral
    , regex "წერტილი|მთელი"
    , Predicate $ not . hasGrain
    ]
  , prod = \case
      (Token Numeral nd1:_:Token Numeral nd2:_) ->
        double $ TNumeral.value nd1 + decimalsToDouble (TNumeral.value nd2)
      _ -> Nothing
  }

ruleLeadingDotSpelledOut :: Rule
ruleLeadingDotSpelledOut = Rule
  { name = "point 77"
  , pattern =
    [ regex "წერტილი|მთელი"
    , Predicate $ not . hasGrain
    ]
  , prod = \case
      (_:Token Numeral nd:_) -> double . decimalsToDouble $ TNumeral.value nd
      _ -> Nothing
  }

ruleDecimals :: Rule
ruleDecimals = Rule
  { name = "decimal number"
  , pattern =
    [ regex "(\\d*\\.\\d+)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) -> parseDecimal True match
      _ -> Nothing
  }

ruleCommas :: Rule
ruleCommas = Rule
  { name = "comma-separated numbers"
  , pattern =
    [ regex "(\\d+(,\\d\\d\\d)+(\\.\\d+)?)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDouble (Text.replace "," Text.empty match) >>= double
      _ -> Nothing
  }

ruleSuffixes :: Rule
ruleSuffixes = Rule
  { name = "suffixes (K,M,G))"
  , pattern =
    [ dimension Numeral
    , regex "(k|m|g)(?=[\\W$€¢£]|$)"
    ]
  , prod = \case
      (Token Numeral nd : Token RegexMatch (GroupMatch (match : _)):_) -> do
        x <- case Text.toLower match of
          "k" -> Just 1e3
          "m" -> Just 1e6
          "g" -> Just 1e9
          _ -> Nothing
        double $ TNumeral.value nd * x
      _ -> Nothing
  }

ruleNegative :: Rule
ruleNegative = Rule
  { name = "negative numbers"
  , pattern =
    [ regex "(-|მინუს|მინ)(?!\\s*-)"
    , Predicate isPositive
    ]
  , prod = \case
      (_:Token Numeral nd:_) -> double (TNumeral.value nd * (-1))
      _ -> Nothing
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

ruleSumAnd :: Rule
ruleSumAnd = Rule
  { name = "intersect 2 numbers (with and)"
  , pattern =
    [ Predicate $ and . sequence [hasGrain, isPositive]
    , regex "და"
    , Predicate $ and . sequence [not . isMultipliable, isPositive]
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = val1, TNumeral.grain = Just g}:
       _:
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
  [ ruleToNineteen
  , ruleHundreds
  , ruleCompositeHundreds
  , ruleCompositeHundredsAndUnits
  , ruleCompositeHundredsAndTens
  , ruleTens
  , rulePowersOfTen
  , ruleCompositeTens
  , ruleDotSpelledOut
  , ruleLeadingDotSpelledOut
  , ruleDecimals
  , ruleCommas
  , ruleSuffixes
  , ruleNegative
  , ruleSum
  , ruleSumAnd
  , ruleMultiply
  ]
