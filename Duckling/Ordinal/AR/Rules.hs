-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Ordinal.AR.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Prelude
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Helpers
import Duckling.Regex.Types
import Duckling.Types

ordinalsMap :: HashMap Text Int
ordinalsMap = HashMap.fromList
  [ ( "اول", 1 )
  , ( "أول", 1 )
  , ( "حاد", 1 )
  , ( "حادي", 1 )
  , ( "واحد", 1 )
  , ( "ثان", 2 )
  , ( "ثاني", 2 )
  , ( "ثالث", 3 )
  , ( "رابع", 4 )
  , ( "خامس", 5 )
  , ( "سادس", 6 )
  , ( "سابع", 7 )
  , ( "ثامن", 8 )
  , ( "تاسع", 9 )
  , ( "عاشر", 10 )
  ]

-- حذفنا ون،ين للتوحيد بين المذكر والمؤنث
cardinalsMap :: HashMap Text Int
cardinalsMap = HashMap.fromList
  [ ( "عشر", 20 )
  , ( "ثلاث", 30 )
  , ( "اربع", 40 )
  , ( "خمس", 50 )
  , ( "ست", 60 )
  , ( "سبع", 70 )
  , ( "ثمان", 80 )
  , ( "تسع", 90 )
  ]

ruleCompositeOrdinals :: Rule
ruleCompositeOrdinals = Rule
  { name = "ordinals (composite, e.g., eighty-seven)"
  , pattern =
    [ regex "ال(واحد|حادي?|ثاني?|ثالث|رابع|خامس|سادس|سابع|ثامن|تاسع|عاشر) و ?ال(عشر|ثلاث|اربع|خمس|ست|سبع|ثمان|تسع)(ون|ين)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (tens:units:_)):_) -> do
        tt <- HashMap.lookup (Text.toLower tens) ordinalsMap
        uu <- HashMap.lookup (Text.toLower units) cardinalsMap
        Just . ordinal $ tt + uu
      _ -> Nothing
  }

ruleOrdinals1To10 :: Rule
ruleOrdinals1To10 = Rule
  { name = "ordinals (first..tenth)"
  , pattern =
    [ regex "(?:ال)?([أا]ول|ثاني?|ثالث|رابع|خامس|سادس|سابع|ثامن|تاسع|عاشر)[ةهى]?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> HashMap.lookup (Text.toLower match) ordinalsMap
      _ -> Nothing
  }

ruleOrdinals11 :: Rule
ruleOrdinals11 = Rule
  { name = "ordinals (eleventh)"
  , pattern =
    [ regex "ال([اأإ]حد[يى]?|حاد(ي[ةه]?)?) ?عشر[ةه]?"
    ]
  , prod = \_ -> Just $ ordinal 11
  }

ruleOrdinals12 :: Rule
ruleOrdinals12 = Rule
  { name = "ordinals (twelveth)"
  , pattern =
    [ regex "ال([اأإ]ثن[يى]?|ثان(ي[ةه]?)?) ?عشر[ةه]?"
    ]
  , prod = \_ -> Just $ ordinal 12
  }

ruleOrdinals13To19 :: Rule
ruleOrdinals13To19 = Rule
  { name = "ordinals (thirtieth..nineteenth)"
  , pattern =
    [ regex "ال(ثالث|رابع|خامس|سادس|سابع|ثامن|تاسع)[ةه]? ?عشرة?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        uu <- HashMap.lookup (Text.toLower match) ordinalsMap
        Just . ordinal $ 10 + uu
      _ -> Nothing
  }

ruleOrdinals3 :: Rule
ruleOrdinals3 = Rule
  { name = "ordinals (twenty, thirty..ninety)"
  , pattern =
    [ regex "ال(عشر|ثلاث|اربع|خمس|ست|سبع|ثمان|تسع)(ون|ين)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> HashMap.lookup (Text.toLower match) cardinalsMap
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleCompositeOrdinals
  , ruleOrdinals1To10
  , ruleOrdinals11
  , ruleOrdinals12
  , ruleOrdinals13To19
  , ruleOrdinals3
  ]
