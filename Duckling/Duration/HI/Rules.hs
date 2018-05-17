-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.HI.Rules
  ( rules
  ) where

import Control.Monad (join)
import qualified Data.Text as Text
import Prelude
import Data.String
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.HashMap.Strict as HashMap

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers
import Duckling.Numeral.Helpers (parseInteger)
import Duckling.Numeral.Types (NumeralData (..))
import qualified Duckling.Numeral.Types as TNumeral
import Duckling.Regex.Types
import qualified Duckling.TimeGrain.Types as TG
import Duckling.Types

avadhiMap :: HashMap Text TG.Grain
avadhiMap = HashMap.fromList
  [ ("साल"  , TG.Year)
  , ("वर्ष", TG.Year)
  , ("महीना", TG.Month)
  , ("दिन"   , TG.Day)
  , ("दिवस"   , TG.Day)
  , ("घंटा"  , TG.Hour)
  , ("मिनट", TG.Minute)
  ]

ruleAadha :: Rule
ruleAadha = Rule
  { name = "half of a duration"
  , pattern =
    [ regex "आधा ((साल|वर्ष)|(महीना)|(दिन|दिवस)|(घंटा)|(मिनट))"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (x:_)):_) -> do
        grain <- HashMap.lookup (Text.toLower x) avadhiMap
        Token Duration <$> timesOneAndAHalf grain 0
      _ -> Nothing
  }

ruleDurationPandrahMinat :: Rule
ruleDurationPandrahMinat = Rule
  { name = "quarter of an hour"
  , pattern =
    [ regex "पंद्रह मिनट"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Minute 15
  }

ruleDurationPakhwaada :: Rule
ruleDurationPakhwaada = Rule
  { name = "fortnight"
  , pattern =
    [ regex "((एक पखवाड़ा)|(पखवाड़ा))"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Day 14
  }

ruleDurationDin :: Rule
ruleDurationDin = Rule
  { name = "a day"
  , pattern =
    [ regex "((एक दिन)|(एक दिवस)|(दिन|दिवस))"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Day 1
  }

ruleDurationEkSaal :: Rule
ruleDurationEkSaal = Rule
  { name = "one year"
  , pattern =
    [ regex "एक (साल|वर्ष)"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Year 1
  }

ruleDurationPreciseImprecise :: Rule
ruleDurationPreciseImprecise = Rule
  { name = "about|exactly <duration>"
  , pattern =
    [ regex "(लगभग|बिल्कुल|केवल)"
    , dimension Duration
    ]
    , prod = \tokens -> case tokens of
        (_:token:_) -> Just token
        _ -> Nothing
  }


rules :: [Rule]
rules =
  [  ruleDurationPandrahMinat
  ,  ruleDurationPakhwaada
  ,  ruleDurationDin
  ,  ruleAadha
  ,  ruleDurationEkSaal
  ,  ruleDurationPreciseImprecise
  ]
