-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE LambdaCase #-}

module Duckling.Numeral.BN.Rules
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

bengaliMap :: HashMap Char Char
bengaliMap = HashMap.fromList
  [ ( '০', '0' )
  , ( '১', '1' )
  , ( '২', '2' )
  , ( '৩', '3' )
  , ( '৪', '4' )
  , ( '৫', '5' )
  , ( '৬', '6' )
  , ( '৭', '7' )
  , ( '৮', '8' )
  , ( '৯', '9' )
  ]

bengaliToArab :: Char -> Char
bengaliToArab c = HashMap.lookupDefault c c bengaliMap

ruleBengali :: Rule
ruleBengali = Rule
  { name = "bengali forms"
  , pattern =
    [ regex "([০১২৩৪৫৬৭৮৯]{1,18})"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        toInteger <$> parseInt (Text.map bengaliToArab match) >>= integer
      _ -> Nothing
}

ruleNumeralMap :: HashMap Text Integer
ruleNumeralMap = HashMap.fromList
  [ ( "শূন্য", 0 )
  , ( "এক", 1 )
  , ( "দুই" , 2 )
  , ( "তিন", 3 )
  , ( "চার", 4 )
  , ( "পাঁচ", 5 )
  , ( "ছয়", 6 )
  , ( "সাত", 7 )
  , ( "আট", 8 )
  , ( "নয়" , 9 )
  , ( "দশ", 10 )
  ]

ruleNumeral :: Rule
ruleNumeral = Rule
  { name = "number (0..10)"
  , pattern =
    [ regex "(শূন্য|এক|দুই|তিন|চার|পাঁচ|ছয়|সাত|আট|নয়|দশ)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup match ruleNumeralMap >>= integer
      _ -> Nothing
}

elevenToNineteenMap :: HashMap Text Integer
elevenToNineteenMap = HashMap.fromList
  [ ( "এগারো", 11 )
  , ( "বারো", 12 )
  , ( "তেরো", 13 )
  , ( "চৌদ্দ", 14 )
  , ( "পনেরো", 15 )
  , ( "ষোল", 16 )
  , ( "সতেরো", 17 )
  , ( "আঠারো", 18 )
  , ( "উনিশ", 19 )
  ]

ruleElevenToNineteen :: Rule
ruleElevenToNineteen = Rule
  { name = "number (11..19)"
  , pattern =
    [ regex "(এগারো|বারো|তেরো|চৌদ্দ|পনেরো|ষোল|সতেরো|আঠারো|উনিশ)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup match elevenToNineteenMap >>= integer
      _ -> Nothing
}

twentyoneToTwentynineMap :: HashMap Text Integer
twentyoneToTwentynineMap = HashMap.fromList
  [ ( "একুশ", 21 )
  , ( "বাইশ", 22 )
  , ( "তেইশ", 23 )
  , ( "চব্বিশ", 24 )
  , ( "পঁচিশ", 25 )
  , ( "ছাব্বিশ", 26 )
  , ( "সাতাশ", 27 )
  , ( "আঠাশ", 28 )
  , ( "ঊনত্রিশ", 29 )
  ]

ruleTwentyoneToTwentynine :: Rule
ruleTwentyoneToTwentynine = Rule
  { name = "number (21..29)"
  , pattern =
    [ regex "(একুশ|বাইশ|তেইশ|চব্বিশ|পঁচিশ|ছাব্বিশ|সাতাশ|আঠাশ|ঊনত্রিশ)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup match twentyoneToTwentynineMap >>= integer
      _ -> Nothing
}

tensMap :: HashMap Text Integer
tensMap = HashMap.fromList
  [ ( "কুড়ি", 20 )
  , ( "তিরিশ", 30 )
  , ( "চল্লিশ", 40 )
  , ( "পঞ্চাশ", 50 )
  , ( "ষাট", 60 )
  , ( "সত্তর", 70 )
  , ( "আশি", 80 )
  , ( "নব্বই", 90 )
  ]

ruleTens :: Rule
ruleTens = Rule
  { name = "integer (20..90)"
  , pattern =
    [ regex "(কুড়ি|তিরিশ|চল্লিশ|পঞ্চাশ|ষাট|সত্তর|আশি|নব্বই)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup match tensMap >>= integer
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleBengali
  , ruleNumeral
  , ruleElevenToNineteen
  , ruleTwentyoneToTwentynine
  , ruleTens
  ]
