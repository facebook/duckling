-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Ordinal.KA.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Helpers
import Duckling.Regex.Types
import Duckling.Types

ordinalsMap :: HashMap Text Int
ordinalsMap = HashMap.fromList
  [ ("პირველი", 1)
  , ("პირველ", 1)
  , ("მეერთე", 1)
  , ("მეორე", 2)
  , ("მესამე", 3)
  , ("მეოთხე", 4)
  , ("მეხუთე", 5)
  , ("მეექვსე", 6)
  , ("მეშვიდე", 7)
  , ("მერვე", 8)
  , ("მეცხრე", 9)
  , ("მეათე", 10)
  , ("მეთერთმეტე", 11)
  , ("მეთორმეტე", 12)
  , ("მეცამეტე", 13)
  , ("მეთოთხმეტე", 14)
  , ("მეთხუთმეტე", 15)
  , ("მეთქვსმეტე", 16)
  , ("მეჩვიდმეტე", 17)
  , ("მეთვრამეტე", 18)
  , ("მეცხრამეტე", 19)
  , ("მეოცე", 20)
  , ("ოცდამეათე", 30)
  , ("მეორმოცე", 40)
  , ("ორმოცდამეათე", 50)
  , ("მესამოცე", 60)
  , ("სამოცდამეათე", 70)
  , ("მეოთხმოცე", 80)
  , ("ოთხმოცდამეათე", 90)
  ]

cardinalsMap :: HashMap Text Int
cardinalsMap = HashMap.fromList
  [ ("ოცი", 20)
  , ("ოცდა", 20)
  , ("ოცდაათი", 30)
  , ("ორმოცი", 40)
  , ("ორმოცდა", 40)
  , ("ორმოცდაათი", 50)
  , ("სამოცი", 60)
  , ("სამოცდა", 60)
  , ("სამოცდაათი", 70)
  , ("ოთხმოცი", 80)
  , ("ოთხმოცდა", 80)
  , ("ოთხმოცდაათი", 90)
  ]

ruleOrdinals :: Rule
ruleOrdinals = Rule
  { name = "ordinals (first..twentieth,thirtieth,...)"
  , pattern =
    [ regex "(პირველი?|მეორე|მესამე|მეოთხე|მეხუთე|მეექვსე|მეშვიდე|მერვე|მეცხრე|მეათე|მეთერთმეტე|მეთოთხმეტე|მეცამეტე|მეთოთხმეტე|მეთხუთმეტე|მეთექვსმეტე|მეჩვიდმეტე|მეთვრამეტე|მეცხრამეტე|მეოცე|ოცდამეათე|მეორმოცე|ორმოცდამეათე|მესამოცე|სამოცდამეათე|მეოთხმოცე|ოთხმოცდამეათე)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> HashMap.lookup (Text.toLower match) ordinalsMap
      _ -> Nothing
    }

ruleCompositeOrdinals :: Rule
ruleCompositeOrdinals = Rule
  { name = "ordinals (composite, e.g. eighty-seven, forty—seventh, twenty ninth, thirtythird)"
  , pattern =
    [ regex "(ოცდა|ორმოცდა|სამოცდა|ოთხმოცდა)[\\s\\-\\—]?(მეერთე|მეორე|მესამე|მეოთხე|მეხუთე|მეექვსე|მეშვიდე|მერვე|მეცხრე|მეათე|მეთერთმეტე|მეთოთხმეტე|მეცამეტე|მეთოთხმეტე|მეთხუთმეტე|მეთექვსმეტე|მეჩვიდმეტე|მეთვრამეტე|მეცხრამეტე)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (tens:units:_)):_) -> do
        tt <- HashMap.lookup (Text.toLower tens) cardinalsMap
        uu <- HashMap.lookup (Text.toLower units) ordinalsMap
        Just (ordinal (tt + uu))
      _ -> Nothing
  }

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern =
    [ regex "0*(\\d+) ?(-ლი|-ე)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) -> ordinal <$> parseInt match
      _ -> Nothing
  }

ruleOrdinalDigits1 :: Rule
ruleOrdinalDigits1 = Rule
  { name = "ordinal (digits)"
  , pattern =
    [ regex "მე-? ?0*(\\d+)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) -> ordinal <$> parseInt match
      _ -> Nothing
  }

ruleOrdinalDigits2 :: Rule
ruleOrdinalDigits2 = Rule
  { name = "ordinal (digits)"
  , pattern =
    [ regex "მე-? ?0*(\\d+) ?(-ლი|-ე)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) -> ordinal <$> parseInt match
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleOrdinals
  , ruleCompositeOrdinals
  , ruleOrdinalDigits
  , ruleOrdinalDigits1
  , ruleOrdinalDigits2
  ]
