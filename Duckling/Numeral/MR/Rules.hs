-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Numeral.MR.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.Maybe
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

marathiDigitMap :: HashMap Char Char
marathiDigitMap = HashMap.fromList
  [ ( '०', '0' )
  , ( '१', '1' )
  , ( '२', '2' )
  , ( '३', '3' )
  , ( '४', '4' )
  , ( '५', '5' )
  , ( '६', '6' )
  , ( '७', '7' )
  , ( '८', '8' )
  , ( '९', '9' )
  ]

marathiToArab :: Char -> Char
marathiToArab c = HashMap.lookupDefault c c marathiDigitMap

ruleMarathiDigits :: Rule
ruleMarathiDigits = Rule
  { name = "marathi digits"
  , pattern =
    [ regex "([०१२३४५६७८९]{1,18})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        toInteger <$> parseInt (Text.map marathiToArab match) >>= integer
      _ -> Nothing
  }

ruleInteger :: Rule
ruleInteger = Rule
  { name = "integer (numeric)"
  , pattern =
    [ regex "(\\d{1,18})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        toInteger <$> parseInt match >>= integer
      _ -> Nothing
  }

ruleZeroToNinetyNineMap :: HashMap Text Integer
ruleZeroToNinetyNineMap = HashMap.fromList
  [ ( "शून्य", 0 )
  , ( "एक", 1 )
  , ( "दोन", 2 )
  , ( "तीन", 3 )
  , ( "चार", 4 )
  , ( "पाच", 5 )
  , ( "सहा", 6 )
  , ( "सात", 7 )
  , ( "आठ", 8 )
  , ( "नऊ", 9 )
  , ( "दहा", 10 )
  , ( "अकरा", 11 )
  , ( "बारा", 12 )
  , ( "तेरा", 13 )
  , ( "चौदा", 14 )
  , ( "पंधरा", 15 )
  , ( "सोळा", 16 )
  , ( "सतरा", 17 )
  , ( "अठरा", 18 )
  , ( "एकोणीस", 19 )
  , ( "वीस", 20 )
  , ( "एकवीस", 21 )
  , ( "बावीस", 22 )
  , ( "तेवीस", 23 )
  , ( "चोवीस", 24 )
  , ( "पंचवीस", 25 )
  , ( "सव्वीस", 26 )
  , ( "सत्तावीस", 27 )
  , ( "अठ्ठावीस", 28 )
  , ( "एकोणतीस", 29 )
  , ( "तीस", 30 )
  , ( "चाळीस", 40 )
  , ( "पन्नास", 50 )
  , ( "साठ", 60 )
  , ( "सत्तर", 70 )
  , ( "ऐंशी", 80 )
  , ( "नव्वद", 90 )
  ]

ruleZeroToNinetyNine :: Rule
ruleZeroToNinetyNine = Rule
  { name = "number (0..99)"
  , pattern =
    [ regex "((?:शून्य|एक|दोन|तीन|चार|पाच|सहा|सात|आठ|नऊ|दहा|अकरा|बारा|तेरा|चौदा|पंधरा|सोळा|सतरा|अठरा|एकोणीस|वीस|एकवीस|बावीस|तेवीस|चोवीस|पंचवीस|सव्वीस|सत्तावीस|अठ्ठावीस|एकोणतीस|तीस|चाळीस|पन्नास|साठ|सत्तर|ऐंशी|नव्वद)(?:ांवरून|वरून|ांवर|वर|ांपर्यंत|ंपर्यंत|पर्यंत|ापासून|पासून|ंपासून|ांस|ांचा|ांची|ांचे|ांना|ात|ची|चा|चे|ना)?)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        let hasSuffix = Text.length match > 4 && (Text.isSuffixOf "वरून" match || Text.isSuffixOf "वर" match || Text.isSuffixOf "पर्यंत" match || Text.isSuffixOf "ंपर्यंत" match || Text.isSuffixOf "पासून" match || Text.isSuffixOf "ंपासून" match || Text.isSuffixOf "ापासून" match || Text.isSuffixOf "ांपासून" match || Text.isSuffixOf "चा" match || Text.isSuffixOf "ची" match || Text.isSuffixOf "चे" match)
            findValue [] = Nothing
            findValue (numText:xs) =
              if Text.isPrefixOf numText match
                then HashMap.lookup numText ruleZeroToNinetyNineMap >>= integer
                else findValue xs
            baseRes = findValue (HashMap.keys ruleZeroToNinetyNineMap)
        in if hasSuffix
             then baseRes >>= notOkForAnyTime
             else baseRes
      _ -> Nothing
  }

rulePowersOfTen :: Rule
rulePowersOfTen = Rule
  { name = "powers of tens"
  , pattern = [regex "((?:शंभर|शे|हजार|हजारा|लाख|लाखा|कोटी|कोटीं|करोड)(?:ापासून|पासून|ंपासून|ांपासून|ांपर्यंत|ंपर्यंत|पर्यंत|ांवरून|वरून|ांवर|वर|ांस|ांचा|ांची|ांचे|ांना|ात|ची|चा|चे|ना)?)"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match : _)) : _) ->
        let hasSuffix = Text.length match > 4 && (Text.isSuffixOf "वरून" match || Text.isSuffixOf "वर" match || Text.isSuffixOf "पर्यंत" match || Text.isSuffixOf "ंपर्यंत" match || Text.isSuffixOf "पासून" match || Text.isSuffixOf "ंपासून" match || Text.isSuffixOf "ापासून" match || Text.isSuffixOf "ांपासून" match || Text.isSuffixOf "चा" match || Text.isSuffixOf "ची" match || Text.isSuffixOf "चे" match)
            baseRes = if Text.isPrefixOf "शंभर" match then double 1e2 >>= withGrain 2 >>= withMultipliable
                      else if Text.isPrefixOf "शे" match then double 1e2 >>= withGrain 2 >>= withMultipliable
                      else if Text.isPrefixOf "हजार" match then double 1e3 >>= withGrain 3 >>= withMultipliable
                      else if Text.isPrefixOf "लाख" match then double 1e5 >>= withGrain 5 >>= withMultipliable
                      else if Text.isPrefixOf "कोटी" match then double 1e7 >>= withGrain 7 >>= withMultipliable
                      else if Text.isPrefixOf "करोड" match then double 1e7 >>= withGrain 7 >>= withMultipliable
                      else Nothing
        in if hasSuffix
             then baseRes >>= notOkForAnyTime
             else baseRes
      _ -> Nothing
  }

ruleMultiply :: Rule
ruleMultiply = Rule
  { name = "compose by multiplication"
  , pattern =
    [ Predicate $ and . sequence [isPositive, isOkForAnyTime]
    , Predicate isMultipliable
    ]
  , prod = \tokens -> case tokens of
      (token1:token2:_) -> multiply token1 token2
      _ -> Nothing
  }

ruleDecimals :: Rule
ruleDecimals = Rule
  { name = "decimal number"
  , pattern =
    [ regex "(\\d*\\.\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> parseDecimal True match
      _ -> Nothing
  }

ruleMarathiDecimals :: Rule
ruleMarathiDecimals = Rule
  { name = "marathi decimals"
  , pattern =
    [ regex "([०१२३४५६७८९]+[\\.,][०१२३४५६७८९]+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDecimal True (Text.map marathiToArab match)
      _ -> Nothing
  }

ruleDotSpelledOut :: Rule
ruleDotSpelledOut = Rule
  { name = "one point 2"
  , pattern =
    [ dimension Numeral
    , regex "पॉइंट|पॉईंट|दशांश"
    , Predicate $ not . hasGrain
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral nd1:_:Token Numeral nd2:_) ->
        double $ TNumeral.value nd1 + decimalsToDouble (TNumeral.value nd2)
      _ -> Nothing
  }

ruleLeadingDotSpelledOut :: Rule
ruleLeadingDotSpelledOut = Rule
  { name = "point 77"
  , pattern =
    [ regex "पॉइंट|पॉईंट|दशांश"
    , Predicate $ not . hasGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral nd:_) -> double . decimalsToDouble $ TNumeral.value nd
      _ -> Nothing
  }

ruleFractionsMap :: HashMap Text Double
ruleFractionsMap = HashMap.fromList
  [ ( "अर्धा", 0.5 )
  , ( "अर्धी", 0.5 )
  , ( "अर्धे", 0.5 )
  , ( "पाव", 0.25 )
  , ( "पावणे", 0.75 )
  , ( "सव्वा", 1.25 )
  , ( "दीड", 1.5 )
  , ( "अडीच", 2.5 )
  ]

ruleFractions :: Rule
ruleFractions = Rule
  { name = "fractional numbers"
  , pattern =
    [ regex "(?:^|\\s)(अर्धा|अर्धी|अर्धे|पाव|पावणे|सव्वा|दीड|अडीच)(?=$|\\s)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup match ruleFractionsMap >>= double
      _ -> Nothing
  }

ruleDozen :: Rule
ruleDozen = Rule
  { name = "dozen"
  , pattern =
    [ regex "डझन"
    ]
  , prod = \_ -> integer 12 >>= withMultipliable
  }

ruleCommas :: Rule
ruleCommas = Rule
  { name = "comma-separated numbers"
  , pattern =
    [ regex "([0-9]{1,3}(?:,[0-9]{2,3})+|[०-९]{1,3}(?:,[०-९]{2,3})+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDecimal True (Text.map marathiToArab $ Text.replace "," Text.empty match)
      _ -> Nothing
  }

ruleDenominatorFractionsMap :: HashMap Text Double
ruleDenominatorFractionsMap = HashMap.fromList
  [ ( "तृतीयांश", 3 )
  , ( "चतुर्थांश", 4 )
  , ( "पंचमांश", 5 )
  , ( "षष्ठांश", 6 )
  , ( "सप्तमांश", 7 )
  , ( "अष्टमांश", 8 )
  , ( "नवमांश", 9 )
  , ( "दशमांश", 10 )
  , ( "शतांश", 100 )
  , ( "सहस्त्रांश", 1000 )
  ]

ruleNumeratorDenominatorFraction :: Rule
ruleNumeratorDenominatorFraction = Rule
  { name = "<numerator> <denominator-fraction>"
  , pattern =
    [ dimension Numeral
    , regex "(तृतीयांश|चतुर्थांश|पंचमांश|षष्ठांश|सप्तमांश|अष्टमांश|नवमांश|दशमांश|शतांश|सहस्त्रांश)"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> do
        denom <- HashMap.lookup match ruleDenominatorFractionsMap
        double (v / denom)
      _ -> Nothing
  }

ruleStandaloneDenominatorFraction :: Rule
ruleStandaloneDenominatorFraction = Rule
  { name = "<denominator-fraction>"
  , pattern =
    [ regex "(तृतीयांश|चतुर्थांश|पंचमांश|षष्ठांश|सप्तमांश|अष्टमांश|नवमांश|दशमांश|शतांश|सहस्त्रांश)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        denom <- HashMap.lookup match ruleDenominatorFractionsMap
        double (1 / denom)
      _ -> Nothing
  }

ruleFractionChed :: Rule
ruleFractionChed = Rule
  { name = "<number> छेद|भागिले <number>"
  , pattern =
    [ dimension Numeral
    , regex "छेद|भागिले"
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (t1:_:t2:_) -> divide t1 t2
      _ -> Nothing
  }

ruleBetweenNumerals :: Rule
ruleBetweenNumerals = Rule
  { name = "<number> ते/आणि <number>"
  , pattern =
    [ dimension Numeral
    , regex "તે|ते|(पासून )?તે|(पासून )?ते|आणि|-"
    , dimension Numeral
    ]
  , prod = \case
      (t1:_:t2:_) -> betweenNumerals t1 t2
      _ -> Nothing
  }

ruleBetweenNumeralsSuffix :: Rule
ruleBetweenNumeralsSuffix = Rule
  { name = "<number> ते <number> पर्यंत"
  , pattern =
    [ dimension Numeral
    , regex "તે|ते|(पासून )?તે|(पासून )?ते|पर्यंत|आणि|-"
    , dimension Numeral
    , regex "पर्यंत"
    ]
  , prod = \case
      (t1:_:t2:_:_) -> betweenNumerals t1 t2
      _ -> Nothing
  }

ruleBetweenNumeralsSuffixDirect :: Rule
ruleBetweenNumeralsSuffixDirect = Rule
  { name = "<number-from> <number-to>"
  , pattern =
    [ Predicate $ not . isOkForAnyTime
    , dimension Numeral
    ]
  , prod = \case
      (t1:t2:_) -> betweenNumerals t1 t2
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleMarathiDigits
  , ruleInteger
  , ruleZeroToNinetyNine
  , rulePowersOfTen
  , ruleDozen
  , ruleMultiply
  , ruleCommas
  , ruleBetweenNumerals
  , ruleBetweenNumeralsSuffix
  , ruleBetweenNumeralsSuffixDirect
  , ruleDecimals
  , ruleMarathiDecimals
  , ruleDotSpelledOut
  , ruleLeadingDotSpelledOut
  , ruleFractions
  , ruleNumeratorDenominatorFraction
  , ruleStandaloneDenominatorFraction
  , ruleFractionChed
  ]
