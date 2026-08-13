-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.MR.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.Text as Text
import Prelude
import qualified Data.HashMap.Strict as HashMap

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Helpers
import Duckling.Regex.Types
import Duckling.Types

ordinalsMap :: HashMap Text Int
ordinalsMap = HashMap.fromList
  [ ( "प्रथम", 1 )
  , ( "पहिला", 1 )
  , ( "पहिली", 1 )
  , ( "पहिले", 1 )
  , ( "पहिल्या", 1 )
  , ( "द्वितीय", 2 )
  , ( "दुसरा", 2 )
  , ( "दुसरी", 2 )
  , ( "दुसरे", 2 )
  , ( "दुसऱ्या", 2 )
  , ( "तृतीय", 3 )
  , ( "तिसरा", 3 )
  , ( "तिसरी", 3 )
  , ( "तिसरे", 3 )
  , ( "तिसऱ्या", 3 )
  , ( "चौथा", 4 )
  , ( "चौथी", 4 )
  , ( "चौथे", 4 )
  , ( "चौथ्या", 4 )
  , ( "पाचवा", 5 )
  , ( "पाचवी", 5 )
  , ( "पाचवे", 5 )
  , ( "पाचव्या", 5 )
  , ( "सहावा", 6 )
  , ( "सहावी", 6 )
  , ( "सहावे", 6 )
  , ( "सहाव्या", 6 )
  , ( "सातवा", 7 )
  , ( "सातवी", 7 )
  , ( "सातवे", 7 )
  , ( "सातव्या", 7 )
  , ( "आठवा", 8 )
  , ( "आठवी", 8 )
  , ( "आठवे", 8 )
  , ( "आठव्या", 8 )
  , ( "नववा", 9 )
  , ( "नववी", 9 )
  , ( "नववे", 9 )
  , ( "नवव्या", 9 )
  , ( "दहावा", 10 )
  , ( "दहावी", 10 )
  , ( "दहावे", 10 )
  , ( "दहाव्या", 10 )
  ]

ruleOrdinals :: Rule
ruleOrdinals = Rule
  { name = "ordinals (first..tenth)"
  , pattern =
    [ regex "(प्रथम|पहिला|पहिली|पहिले|पहिल्या|द्वितीय|दुसरा|दुसरी|दुसरे|दुसऱ्या|तृतीय|तिसरा|तिसरी|तिसरे|तिसऱ्या|चौथा|चौथी|चौथे|चौथ्या|पाचवा|पाचवी|पाचवे|पाचव्या|सहावा|सहावी|सहावे|सहाव्या|सातवा|सातवी|सातवे|सातव्या|आठवा|आठवी|आठवे|आठव्या|नववा|नववी|नववे|नवव्या|दहावा|दहावी|दहावे|दहाव्या)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> HashMap.lookup match ordinalsMap
      _ -> Nothing
  }

cardinalsMap :: HashMap Text Int
cardinalsMap = HashMap.fromList
  [ ( "अकरा", 11 )
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
  , ( "एकोणिसा", 19 )
  , ( "एकोणीसा", 19 )
  , ( "विसा", 20 )
  , ( "एकविसा", 21 )
  , ( "बाविसा", 22 )
  , ( "तेविसा", 23 )
  , ( "चोविसा", 24 )
  , ( "पंचविसा", 25 )
  , ( "सव्विसा", 26 )
  , ( "सत्ताविसा", 27 )
  , ( "अठ्ठाविसा", 28 )
  , ( "तिसा", 30 )
  , ( "तीसा", 30 )
  ]

ruleOtherOrdinals :: Rule
ruleOtherOrdinals = Rule
  { name = "ordinals (eleventh..thirtieth)"
  , pattern =
    [ regex "(अकरा|बारा|तेरा|चौदा|पंधरा|सोळा|सतरा|अठरा|एकोणीस|वीस|एकवीस|बावीस|तेवीस|चोवीस|पंचवीस|सव्वीस|सत्तावीस|अठ्ठावीस|एकोणतीस|तीस|एकोणिसा|एकोणीसा|विसा|एकविसा|बाविसा|तेविसा|चोविसा|पंचविसा|सव्विसा|सत्ताविसा|अठ्ठाविसा|तिसा|तीसा)(वा|वी|वे|व्या)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> HashMap.lookup match cardinalsMap
      _ -> Nothing
  }

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

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern = [regex "0*([\\d०-९]+) ?(वा|वी|वे|व्या)" ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        let clean = Text.map (\c -> HashMap.lookupDefault c c marathiDigitMap) match
        in ordinal <$> parseInt clean
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleOrdinals
  , ruleOtherOrdinals
  , ruleOrdinalDigits
  ]
