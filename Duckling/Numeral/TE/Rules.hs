-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Numeral.TE.Rules
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
import Duckling.Regex.Types
import Duckling.Types

teluguMap :: HashMap Char Char
teluguMap = HashMap.fromList
  [ ( '౦', '0' )
  , ( '౧', '1' )
  , ( '౨', '2' )
  , ( '౩', '3' )
  , ( '౪', '4' )
  , ( '౫', '5' )
  , ( '౬', '6' )
  , ( '౭', '7' )
  , ( '౮', '8' )
  , ( '౯', '9' )
  ]

teluguToArab :: Char -> Char
teluguToArab c = HashMap.lookupDefault c c teluguMap

ruleTelugu :: Rule
ruleTelugu = Rule
  { name = "telugu forms"
  , pattern =
    [ regex "([౦౧౨౩౪౫౬౭౮౯]{1,18})"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        toInteger <$> parseInt (Text.map teluguToArab match) >>= integer
      _ -> Nothing
  }

zeroToNineMap :: HashMap Text Integer
zeroToNineMap = HashMap.fromList
  [ ( "సున్న", 0 )
  , ( "ఒకటి", 1 )
  , ( "రెండు", 2 )
  , ( "మూడు", 3 )
  , ( "నాలుగు", 4 )
  , ( "ఐదు", 5 )
  , ( "ఆరు", 6 )
  , ( "ఏడు", 7 )
  , ( "ఎనిమిది", 8 )
  , ( "తొమ్మిది", 9 )
  ]

ruleZeroToNine :: Rule
ruleZeroToNine = Rule
  { name = "integer (0..9)"
  , pattern =
    [ regex "(సున్న|ఒకటి|రెండు|మూడు|నాలుగు|ఐదు|ఆరు|ఏడు|ఎనిమిది|తొమ్మిది)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) zeroToNineMap >>= integer
      _ -> Nothing
  }

tenToNineteenMap :: HashMap Text Integer
tenToNineteenMap = HashMap.fromList
  [ ( "పది", 10 )
  , ( "పదకొండు", 11 )
  , ( "పన్నెండు", 12 )
  , ( "పదమూడు", 13 )
  , ( "పద్నాల్గు", 14 )
  , ( "పదిహేను", 15 )
  , ( "పదహారు", 16 )
  , ( "పదిహేడు", 17 )
  , ( "పద్దెనిమిది", 18 )
  , ( "పంతొమ్మిది", 19 )
  ]

ruleTenToNineteen :: Rule
ruleTenToNineteen = Rule
  { name = "integer (10..19)"
  , pattern =
    [ regex "(పదకొండు|పన్నెండు|పదమూడు|పద్నాల్గు|పదిహేను|పదహారు|పదిహేడు|పద్దెనిమిది|పంతొమ్మిది|పది)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) tenToNineteenMap >>= integer
      _ -> Nothing
  }

tensMap :: HashMap Text Integer
tensMap = HashMap.fromList
  [ ( "ఇరవై", 20 )
  , ( "ముప్పై", 30 )
  , ( "నలబై", 40 )
  , ( "యాబై", 50 )
  , ( "అరవై", 60 )
  , ( "డెబ్బై", 70 )
  , ( "ఎనబై", 80 )
  , ( "తొంబై", 90 )
  ]

ruleTens :: Rule
ruleTens = Rule
  { name = "integer (20..90)"
  , pattern =
    [ regex "(ఇరవై|ముప్పై|నలబై|యాబై|అరవై|డెబ్బై|ఎనబై|తొంబై)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup match tensMap >>= integer
      _ -> Nothing
  }

hundredsMap :: HashMap Text Integer
hundredsMap = HashMap.fromList
  [ ( "వంద", 100 )
  , ( "వెయ్యి", 1000 )
  , ( "లక్ష", 100000 )
  , ( "కోటి", 10000000 )
  ]

rulehundreds :: Rule
rulehundreds = Rule
  { name = "integer (100,1000,100000,10000000)"
  , pattern =
    [ regex "(వంద|వెయ్యి|లక్ష|కోటి)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) hundredsMap >>= integer
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleTelugu
  , ruleZeroToNine
  , ruleTenToNineteen
  , ruleTens
  , rulehundreds
  ]
