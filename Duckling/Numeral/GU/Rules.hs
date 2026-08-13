-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Numeral.GU.Rules
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

gujaratiDigitMap :: HashMap Char Char
gujaratiDigitMap = HashMap.fromList
  [ ( '૦', '0' )
  , ( '૧', '1' )
  , ( '૨', '2' )
  , ( '૩', '3' )
  , ( '૪', '4' )
  , ( '૫', '5' )
  , ( '૬', '6' )
  , ( '૭', '7' )
  , ( '૮', '8' )
  , ( '૯', '9' )
  ]

gujaratiToArab :: Char -> Char
gujaratiToArab c = HashMap.lookupDefault c c gujaratiDigitMap

ruleGujaratiDigits :: Rule
ruleGujaratiDigits = Rule
  { name = "gujarati digits"
  , pattern =
    [ regex "([૦૧૨૩૪૫૬૭૮૯]{1,18})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        toInteger <$> parseInt (Text.map gujaratiToArab match) >>= integer
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
  [ ( "શૂન્ય", 0 )
  , ( "એક", 1 )
  , ( "બે", 2 )
  , ( "ત્રણ", 3 )
  , ( "ચાર", 4 )
  , ( "પાંચ", 5 )
  , ( "છ", 6 )
  , ( "સાત", 7 )
  , ( "આઠ", 8 )
  , ( "નવ", 9 )
  , ( "દસ", 10 )
  , ( "અગિયાર", 11 )
  , ( "બાર", 12 )
  , ( "તેર", 13 )
  , ( "ચૌદ", 14 )
  , ( "પંદર", 15 )
  , ( "સોળ", 16 )
  , ( "સત્તર", 17 )
  , ( "અઢાર", 18 )
  , ( "ઓગણીસ", 19 )
  , ( "વીસ", 20 )
  , ( "વીશ", 20 )
  , ( "એકવીસ", 21 )
  , ( "બાવીસ", 22 )
  , ( "તેવીસ", 23 )
  , ( "ચોવીસ", 24 )
  , ( "પચ્ચીસ", 25 )
  , ( "છવ્વીસ", 26 )
  , ( "સત્તાવીસ", 27 )
  , ( "અઠ્ઠાવીસ", 28 )
  , ( "ઓગણત્રીસ", 29 )
  , ( "ત્રીસ", 30 )
  , ( "એકત્રીસ", 31 )
  , ( "બત્રીસ", 32 )
  , ( "તેત્રીસ", 33 )
  , ( "ચોત્રીસ", 34 )
  , ( "પાંત્રીસ", 35 )
  , ( "છત્રીસ", 36 )
  , ( "સાડત્રીસ", 37 )
  , ( "અડત્રીસ", 38 )
  , ( "ઓગણચાલીસ", 39 )
  , ( "ચાલીસ", 40 )
  , ( "એકતાલીસ", 41 )
  , ( "બેતાલીસ", 42 )
  , ( "તેતાલીસ", 43 )
  , ( "ચોતાલીસ", 44 )
  , ( "પિસ્તાલીસ", 45 )
  , ( "છેતાલીસ", 46 )
  , ( "સુડતાલીસ", 47 )
  , ( "અડતાલીસ", 48 )
  , ( "ઓગણપચાસ", 49 )
  , ( "પચાસ", 50 )
  , ( "એકાવન", 51 )
  , ( "બાવન", 52 )
  , ( "ત્રેપન", 53 )
  , ( "ચોપન", 54 )
  , ( "પંચાવન", 55 )
  , ( "છપ્પન", 56 )
  , ( "સત્તાવન", 57 )
  , ( "અઠ્ઠાવન", 58 )
  , ( "ઓગણસાઠ", 59 )
  , ( "સાઠ", 60 )
  , ( "એકસઠ", 61 )
  , ( "બાસઠ", 62 )
  , ( "ત્રેસઠ", 63 )
  , ( "ચોસઠ", 64 )
  , ( "પાંસઠ", 65 )
  , ( "છાસઠ", 66 )
  , ( "સડસઠ", 67 )
  , ( "અડસઠ", 68 )
  , ( "ઓગણસિત્તેર", 69 )
  , ( "સિત્તેર", 70 )
  , ( "એકોતેર", 71 )
  , ( "બોતેર", 72 )
  , ( "તોતેર", 73 )
  , ( "ચુમ્મોતેર", 74 )
  , ( "પંચોતેર", 75 )
  , ( "છોતેર", 76 )
  , ( "સત્યોતેર", 77 )
  , ( "અઠ્યોતેર", 78 )
  , ( "ઓગણએંસી", 79 )
  , ( "એંસી", 80 )
  , ( "એક્યાસી", 81 )
  , ( "બ્યાસી", 82 )
  , ( "ત્યાસી", 83 )
  , ( "ચોર્યાસી", 84 )
  , ( "પંચાસી", 85 )
  , ( "છ્યાસી", 86 )
  , ( "સત્યાસી", 87 )
  , ( "અઠ્યાસી", 88 )
  , ( "નવાસી", 89 )
  , ( "નેવુ", 90 )
  , ( "એકાણું", 91 )
  , ( "બાણું", 92 )
  , ( "ત્રાણું", 93 )
  , ( "ચોરાણું", 94 )
  , ( "પંચાણું", 95 )
  , ( "છન્નું", 96 )
  , ( "સત્તાણું", 97 )
  , ( "અઠ્ઠાણું", 98 )
  , ( "નવ્વાણું", 99 )
  ]

ruleZeroToNinetyNine :: Rule
ruleZeroToNinetyNine = Rule
  { name = "number (0..99)"
  , pattern =
    [ regex "(શૂન્ય|એક|બે|ત્રણ|ચાર|પાંચ|છ(?![ા-્અ-હ])|સાત|આઠ|નવ|દસ|અગિયાર|બાર|તેર|ચૌદ|પંદર|સોળ|સત્તર|અઢાર|ઓગણીસ|વીસ|વીશ|એકવીસ|બાવીસ|તેવીસ|ચોવીસ|પચ્ચીસ|છવ્વીસ|સત્તાવીસ|અઠ્ઠાવીસ|ઓગણત્રીસ|ત્રીસ|એકત્રીસ|બત્રીસ|તેત્રીસ|ચોત્રીસ|પાંત્રીસ|છત્રીસ|સાડત્રીસ|અડત્રીસ|ઓગણચાલીસ|ચાલીસ|એકતાલીસ|બેતાલીસ|તેતાલીસ|ચોતાલીસ|પિસ્તાલીસ|છેતાલીસ|સુડતાલીસ|અડતાલીસ|ઓગણપચાસ|પચાસ|એકાવન|બાવન|ત્રેપન|ચોપન|પંચાવન|છપ્પન|સત્તાવન|અઠ્ઠાવન|ઓગણસાઠ|સાઠ|એકસઠ|બાસઠ|ત્રેસઠ|ચોસઠ|પાંસઠ|છાસઠ|સડસઠ|અડસઠ|ઓગણસિત્તેર|સિત્તેર|એકોતેર|બોતેર|તોતેર|ચુમ્મોતેર|પંચોતેર|છોતેર|સત્યોતેર|અઠ્યોતેર|ઓગણએંસી|એંસી|એક્યાસી|બ્યાસી|ત્યાસી|ચોર્યાસી|પંચાસી|છ્યાસી|સત્યાસી|અઠ્યાસી|નવાસી|નેવુ|એકાણું|બાણું|ત્રાણું|ચોરાણું|પંચાણું|છન્નું|સત્તાણું|અઠ્ઠાણું|નવ્વાણું)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup match ruleZeroToNinetyNineMap >>= integer
      _ -> Nothing
  }

rulePowersOfTen :: Rule
rulePowersOfTen = Rule
  { name = "powers of tens"
  , pattern = [regex "((?:સો|હજાર|લાખ|કરોડ)(?:થી|માં|ની|નો|ના|નું|એ)?)"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match : _)) : _) ->
        let hasSuffix = Text.length match > 2 && (Text.isSuffixOf "થી" match || Text.isSuffixOf "માં" match || Text.isSuffixOf "ની" match || Text.isSuffixOf "નો" match || Text.isSuffixOf "ના" match || Text.isSuffixOf "નું" match || Text.isSuffixOf "એ" match)
            core = Text.takeWhile (\c -> c /= 'થ' && c /= 'મ' && c /= 'ન' && c /= 'એ') match
            res = case Text.toLower core of
              "સો" -> double 1e2 >>= withGrain 2 >>= withMultipliable
              "હજાર" -> double 1e3 >>= withGrain 3 >>= withMultipliable
              "લાખ" -> double 1e5 >>= withGrain 5 >>= withMultipliable
              "કરોડ" -> double 1e7 >>= withGrain 7 >>= withMultipliable
              _ -> Nothing
        in if hasSuffix
             then res >>= notOkForAnyTime
             else res
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

ruleGujaratiDecimals :: Rule
ruleGujaratiDecimals = Rule
  { name = "gujarati decimals"
  , pattern =
    [ regex "([૦૧૨૩૪૫૬૭૮૯]*[\\.,][૦૧૨૩૪૫૬૭૮૯]+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDecimal True (Text.map gujaratiToArab match)
      _ -> Nothing
  }

ruleDotSpelledOut :: Rule
ruleDotSpelledOut = Rule
  { name = "one point 2"
  , pattern =
    [ dimension Numeral
    , regex "પોઇન્ટ|પોઈન્ટ|દશાંશ|બિંદુ"
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
    [ regex "પોઇન્ટ|પોઈન્ટ|દશાંશ|બિંદુ"
    , Predicate $ not . hasGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral nd:_) -> double . decimalsToDouble $ TNumeral.value nd
      _ -> Nothing
  }

ruleFractionsMap :: HashMap Text Double
ruleFractionsMap = HashMap.fromList
  [ ( "અડધું", 0.5 )
  , ( "અડધો", 0.5 )
  , ( "અડધી", 0.5 )
  , ( "પા", 0.25 )
  , ( "પોણો", 0.75 )
  , ( "પોણી", 0.75 )
  , ( "પોણું", 0.75 )
  , ( "સવા", 1.25 )
  , ( "દોઢ", 1.5 )
  , ( "અઢી", 2.5 )
  ]

ruleFractions :: Rule
ruleFractions = Rule
  { name = "fractional numbers"
  , pattern =
    [ regex "(?:^|\\s)(અડધું|અડધો|અડધી|પા|પોણો|પોણી|પોણું|સવા|દોઢ|અઢી)(?=$|\\s)"
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
    [ regex "ડઝન"
    ]
  , prod = \_ -> integer 12 >>= withMultipliable
  }

ruleSum :: Rule
ruleSum = Rule
  { name = "intersect 2 numbers"
  , pattern =
    [ Predicate $ and . sequence [hasGrain, isPositive, isOkForAnyTime]
    , Predicate $ and . sequence [not . isMultipliable, isPositive]
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = val1, TNumeral.grain = Just g, TNumeral.okForAnyTime = ok1}:
       Token Numeral NumeralData{TNumeral.value = val2}:
       _) | ok1 && (10 ** fromIntegral g) > val2 -> double $ val1 + val2
      _ -> Nothing
  }

ruleCommas :: Rule
ruleCommas = Rule
  { name = "comma-separated numbers"
  , pattern =
    [ regex "([0-9]{1,3}(?:,[0-9]{2,3})+|[૦-૯]{1,3}(?:,[૦-૯]{2,3})+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDecimal True (Text.map gujaratiToArab $ Text.replace "," Text.empty match)
      _ -> Nothing
  }

ruleBetweenNumerals :: Rule
ruleBetweenNumerals = Rule
  { name = "<number> થી/સુધી <number>"
  , pattern =
    [ dimension Numeral
    , regex "(?:^|\\s)(થી( લઈને)?|સુધી|અને|-)(?=$|\\s)"
    , dimension Numeral
    ]
  , prod = \case
      (t1:_:t2:_) -> betweenNumerals t1 t2
      _ -> Nothing
  }

ruleBetweenNumeralsSuffix :: Rule
ruleBetweenNumeralsSuffix = Rule
  { name = "<number> થી <number> સુધી"
  , pattern =
    [ dimension Numeral
    , regex "(?:^|\\s)(થી( લઈને)?|અને|-)(?=$|\\s)"
    , dimension Numeral
    , regex "(?:^|\\s)સુધી(?=$|\\s)"
    ]
  , prod = \case
      (t1:_:t2:_:_) -> betweenNumerals t1 t2
      _ -> Nothing
  }

ruleBetweenNumeralsSuffixDirect :: Rule
ruleBetweenNumeralsSuffixDirect = Rule
  { name = "<number> <number> સુધી"
  , pattern =
    [ dimension Numeral
    , dimension Numeral
    , regex "(?:^|\\s)સુધી(?=$|\\s)"
    ]
  , prod = \case
      (t1:t2:_) -> betweenNumerals t1 t2
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleGujaratiDigits
  , ruleInteger
  , ruleZeroToNinetyNine
  , rulePowersOfTen
  , ruleDozen
  , ruleMultiply
  , ruleSum
  , ruleCommas
  , ruleBetweenNumerals
  , ruleBetweenNumeralsSuffix
  , ruleBetweenNumeralsSuffixDirect
  , ruleDecimals
  , ruleGujaratiDecimals
  , ruleDotSpelledOut
  , ruleLeadingDotSpelledOut
  , ruleFractions
  ]
