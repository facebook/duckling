-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Numeral.HI.Rules
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

devanagariMap :: HashMap Char Char
devanagariMap = HashMap.fromList
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

devanagariToArab :: Char -> Char
devanagariToArab c = HashMap.lookupDefault c c devanagariMap

ruleDevanagari :: Rule
ruleDevanagari = Rule
  { name = "devanagari forms"
  , pattern =
    [ regex "([०१२३४५६७८९]{1,18})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        toInteger <$> parseInt (Text.map devanagariToArab match) >>= integer
      _ -> Nothing
  }

ruleZeroToNinetyNineMap :: HashMap Text Integer
ruleZeroToNinetyNineMap = HashMap.fromList
  [ ( "शून्य", 0 )
  , ( "एक", 1 )
  , ( "दो", 2 )
  , ( "तीन", 3 )
  , ( "चार", 4 )
  , ( "पाँच", 5 )
  , ( "पांच", 5 )
  , ( "छः", 6 )
  , ( "छह", 6 )
  , ( "सात", 7 )
  , ( "आठ", 8 )
  , ( "नौ", 9 )
  , ( "दस", 10 )
  , ( "ग्यारह", 11 )
  , ( "बारह", 12 )
  , ( "तेरह", 13 )
  , ( "चौदह", 14 )
  , ( "पन्द्रह", 15 )
  , ( "पंद्रह", 15 )
  , ( "सोलह", 16 )
  , ( "सत्रह", 17 )
  , ( "सतरह", 17 )
  , ( "अठारह", 18 )
  , ( "अट्ठारह", 18 )
  , ( "उन्नीस", 19 )
  , ( "बीस", 20 )
  , ( "इक्कीस", 21 )
  , ( "बाईस", 22 )
  , ( "तेईस", 23 )
  , ( "चौबीस", 24 )
  , ( "पच्चीस", 25 )
  , ( "छब्बीस", 26 )
  , ( "सत्ताईस", 27 )
  , ( "अट्ठाईस", 28 )
  , ( "उनतीस", 29 )
  , ( "उन्नतीस", 29 )
  , ( "तीस", 30 )
  , ( "इकतीस", 31 )
  , ( "इकत्तीस", 31 )
  , ( "बत्तीस", 32 )
  , ( "तैंतीस", 33 )
  , ( "चौंतीस", 34 )
  , ( "पैंतीस", 35 )
  , ( "छत्तीस", 36 )
  , ( "सैंतीस", 37 )
  , ( "अड़तीस", 38 )
  , ( "उनतालीस", 39 )
  , ( "उनचालीस", 39 )
  , ( "उन्चालीस", 39 )
  , ( "चालीस", 40 )
  , ( "इकतालीस", 41 )
  , ( "बयालीस", 42 )
  , ( "तैंतालीस", 43 )
  , ( "चौवालीस", 44 )
  , ( "पैंतालीस", 45 )
  , ( "छियालीस", 46 )
  , ( "सैंतालीस", 47 )
  , ( "अड़तालीस", 48 )
  , ( "उनचास", 49 )
  , ( "उन्चास", 49 )
  , ( "पचास", 50 )
  , ( "इक्कावन", 51 )
  , ( "बावन", 52 )
  , ( "तिरेपन", 53 )
  , ( "चौवन", 54 )
  , ( "पचपन", 55 )
  , ( "छप्पन", 56 )
  , ( "सत्तावन", 57 )
  , ( "अट्ठावन", 58 )
  , ( "उनसठ", 59 )
  , ( "उन्सठ", 59 )
  , ( "साठ", 60 )
  , ( "एकसठ", 61 )
  , ( "बासठ", 62 )
  , ( "तिरेसठ", 63 )
  , ( "चौंसठ", 64 )
  , ( "पैंसठ", 65 )
  , ( "छियासठ", 66 )
  , ( "सड़सठ", 67 )
  , ( "अड़सठ", 68 )
  , ( "उनहत्तर", 69 )
  , ( "उन्हत्तर", 69 )
  , ( "सत्तर", 70 )
  , ( "इकहत्तर", 71 )
  , ( "बहत्तर", 72 )
  , ( "तिहत्तर", 73 )
  , ( "चौहत्तर", 74 )
  , ( "पचहत्तर", 75 )
  , ( "छिहत्तर", 76 )
  , ( "सतहत्तर", 77 )
  , ( "अठहत्तर", 78 )
  , ( "उनासी", 79 )
  , ( "उन्यासी", 79 )
  , ( "अस्सी", 80 )
  , ( "इक्यासी", 81 )
  , ( "बयासी", 82 )
  , ( "तिरासी", 83 )
  , ( "चौरासी", 84 )
  , ( "पचासी", 85 )
  , ( "छियासी", 86 )
  , ( "सतासी", 87 )
  , ( "अट्ठासी", 88 )
  , ( "नवासी", 89 )
  , ( "नब्बे", 90 )
  , ( "इक्यानवे", 91 )
  , ( "बानवे", 92 )
  , ( "तिरानवे", 93 )
  , ( "चौरानवे", 94 )
  , ( "पचानवे", 95 )
  , ( "छियानवे", 96 )
  , ( "सत्तानवे", 97 )
  , ( "अट्ठानवे", 98 )
  , ( "निन्यानवे", 99 )
  , ( "निन्यानबे", 99 )
  ]

ruleZeroToNinetyNine :: Rule
ruleZeroToNinetyNine = Rule
  { name = "number (0..99)"
  , pattern =
    [ regex "(शून्य|एक|दो|तीन|चार|पाँच|पांच|छः|छह|सात|आठ|नौ|दस|ग्यारह|बारह|तेरह|चौदह|पन्द्रह|पंद्रह|सोलह|सत्रह|सतरह|अठारह|अट्ठारह|उन्नीस|बीस|इक्कीस|बाईस|तेईस|चौबीस|पच्चीस|छब्बीस|सत्ताईस|अट्ठाईस|उनतीस|उन्नतीस|तीस|इकतीस|इकत्तीस|बत्तीस|तैंतीस|चौंतीस|पैंतीस|छत्तीस|सैंतीस|अड़तीस|उनतालीस|उनचालीस|उन्चालीस|चालीस|इकतालीस|बयालीस|तैंतालीस|चौवालीस|पैंतालीस|छियालीस|सैंतालीस|अड़तालीस|उनचास|उन्चास|पचास|इक्कावन|बावन|तिरेपन|चौवन|पचपन|छप्पन|सत्तावन|अट्ठावन|उनसठ|उन्सठ|साठ|एकसठ|बासठ|तिरेसठ|चौंसठ|पैंसठ|छियासठ|सड़सठ|अड़सठ|उनहत्तर|उन्हत्तर|सत्तर|इकहत्तर|बहत्तर|तिहत्तर|चौहत्तर|पचहत्तर|छिहत्तर|सतहत्तर|अठहत्तर|उनासी|उन्यासी|अस्सी|इक्यासी|बयासी|तिरासी|चौरासी|पचासी|छियासी|सतासी|अट्ठासी|नवासी|नब्बे|इक्यानवे|बानवे|तिरानवे|चौरानवे|पचानवे|छियानवे|सत्तानवे|अट्ठानवे|निन्यानवे|निन्यानबे)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup match ruleZeroToNinetyNineMap >>= integer
      _ -> Nothing
  }

tensMap :: HashMap Text Integer
tensMap = HashMap.fromList
  [ ("दस", 10)
  , ( "बीस", 20 )
  , ( "तीस", 30 )
  , ( "चालीस", 40 )
  , ( "पचास", 50 )
  , ( "साठ", 60 )
  , ( "सत्तर", 70 )
  , ( "अस्सी", 80 )
  , ( "नब्बे", 90 )
  ]

ruleTens :: Rule
ruleTens = Rule
  { name = "integer (20,30..90)"
  , pattern =
    [ regex "(दस|बीस|तीस|चालीस|पचास|साठ|सत्तर|अस्सी|नब्बे)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup match tensMap >>= integer
      _ -> Nothing
  }

rulePowersOfTen :: Rule
rulePowersOfTen = Rule
  { name = "powers of tens"
  , pattern = [regex "(सौ|हजार|हज़ार|हज़ार|लाख|करोड़|करोड)"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match : _)) : _) ->
        case Text.toLower match of
          "सौ" -> double 1e2 >>= withGrain 2 >>= withMultipliable
          "हजार" -> double 1e3 >>= withGrain 3 >>= withMultipliable
          "हज़ार" -> double 1e3 >>= withGrain 3 >>= withMultipliable
          "हज़ार" -> double 1e3 >>= withGrain 3 >>= withMultipliable
          "लाख" -> double 1e5 >>= withGrain 5 >>= withMultipliable
          "करोड़" -> double 1e7 >>= withGrain 7 >>= withMultipliable
          "करोड" -> double 1e7 >>= withGrain 7 >>= withMultipliable
          _ -> Nothing
      _ -> Nothing
  }

ruleMultiply :: Rule
ruleMultiply = Rule
  { name = "compose by multiplication"
  , pattern =
    [ Predicate isPositive
    , Predicate isMultipliable
    ]
  , prod = \tokens -> case tokens of
      (token1:token2:_) -> multiply token1 token2
      _ -> Nothing
  }

ruleCompositeHundreds :: Rule
ruleCompositeHundreds = Rule
  { name = "integer 100s.."
  , pattern =
    [ oneOf [100,200..5000]
    , regex "[\\s\\-]+"
    , Predicate $ numberBetween 1 99
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = hundreds}:
       _:
       Token Numeral NumeralData{TNumeral.value = remaining}:
       _) -> double $ hundreds + remaining
      _ -> Nothing
  }


ruleCompositeThousands :: Rule
ruleCompositeThousands = Rule
  { name = "integer 100s.."
  , pattern =
    [ oneOf [1000,2000..50000]
    , regex "[\\s\\-]+"
    , Predicate $ numberBetween 1 999
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = thousands}:
       _:
       Token Numeral NumeralData{TNumeral.value = remaining}:
       _) -> double $ thousands + remaining
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

ruleDevanagariDecimals :: Rule
ruleDevanagariDecimals = Rule
  { name = "devanagari decimals"
  , pattern =
    [ regex "([०१२३४५६७८९]+[\\.,][०१२३४५६७८९]+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDecimal True (Text.map devanagariToArab match)
      _ -> Nothing
  }

ruleDotSpelledOut :: Rule
ruleDotSpelledOut = Rule
  { name = "one point 2"
  , pattern =
    [ dimension Numeral
    , regex "पॉइंट|दशमलव|बिंदु|दशांश|दशलव"
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
    [ regex "पॉइंट|दशमलव|बिंदु|दशांश|दशलव"
    , Predicate $ not . hasGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral nd:_) -> double . decimalsToDouble $ TNumeral.value nd
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

ruleFractionsMap :: HashMap Text Double
ruleFractionsMap = HashMap.fromList
  [ ( "आधा", 0.5 )
  , ( "आधी", 0.5 )
  , ( "आधे", 0.5 )
  , ( "पाव", 0.25 )
  , ( "पौने", 0.75 )
  , ( "पौना", 0.75 )
  , ( "सवा", 1.25 )
  , ( "डेढ़", 1.5 )
  , ( "ढाई", 2.5 )
  ]

ruleFractions :: Rule
ruleFractions = Rule
  { name = "fractional numbers"
  , pattern =
    [ regex "(?:^|\\s)(आधा|आधी|आधे|पाव|पौने|पौना|सवा|डेढ़|ढाई)(?=$|\\s)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup match ruleFractionsMap >>= double
      _ -> Nothing
  }

ruleSum :: Rule
ruleSum = Rule
  { name = "intersect 2 numbers"
  , pattern =
    [ Predicate $ and . sequence [hasGrain, isPositive]
    , Predicate $ and . sequence [not . isMultipliable, isPositive]
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = val1, TNumeral.grain = Just g}:
       Token Numeral NumeralData{TNumeral.value = val2}:
       _) | (10 ** fromIntegral g) > val2 -> double $ val1 + val2
      _ -> Nothing
  }

ruleDozen :: Rule
ruleDozen = Rule
  { name = "dozen"
  , pattern =
    [ regex "दर्जन(ों)?"
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
        parseDecimal True (Text.map devanagariToArab $ Text.replace "," Text.empty match)
      _ -> Nothing
  }

ruleBetweenNumerals :: Rule
ruleBetweenNumerals = Rule
  { name = "<number> से/तक <number>"
  , pattern =
    [ dimension Numeral
    , regex "से( लेकर)?|तक|और|तथा|-"
    , dimension Numeral
    ]
  , prod = \case
      (t1:_:t2:_) -> betweenNumerals t1 t2
      _ -> Nothing
  }

ruleBetweenNumeralsSuffix :: Rule
ruleBetweenNumeralsSuffix = Rule
  { name = "<number> से <number> तक"
  , pattern =
    [ dimension Numeral
    , regex "से( लेकर)?|और|तथा|-"
    , dimension Numeral
    , regex "तक"
    ]
  , prod = \case
      (t1:_:t2:_:_) -> betweenNumerals t1 t2
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleDevanagari
  , ruleInteger
  , ruleZeroToNinetyNine
  , ruleTens
  , rulePowersOfTen
  , ruleDozen
  , ruleMultiply
  , ruleSum
  , ruleCommas
  , ruleBetweenNumerals
  , ruleBetweenNumeralsSuffix
  , ruleDecimals
  , ruleDevanagariDecimals
  , ruleDotSpelledOut
  , ruleLeadingDotSpelledOut
  , ruleFractions
  ]
