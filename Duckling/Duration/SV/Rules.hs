-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.SV.Rules
  ( rules
  ) where

import Control.Monad (join)
import Data.String
import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers
import Duckling.Numeral.Helpers (parseInteger)
import Duckling.Numeral.Types (NumeralData(..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.TimeGrain.Types as TG

ruleHalfAnHour :: Rule
ruleHalfAnHour = Rule
  { name = "half an hour"
  , pattern =
    [ regex "(1/2|en halv) timme"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Minute 30
  }

ruleIntegerMoreUnitofduration :: Rule
ruleIntegerMoreUnitofduration = Rule
  { name = "<integer> more <unit-of-duration>"
  , pattern =
    [ Predicate isNatural
    , dimension TimeGrain
    , regex "fler|mer"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:
       Token TimeGrain grain:
       _) -> Just . Token Duration . duration grain $ floor v
      _ -> Nothing
  }

ruleNumeralnumberHours :: Rule
ruleNumeralnumberHours = Rule
  { name = "number.number hours"
  , pattern =
    [ regex "(\\d+)\\,(\\d+)"
    , regex "timm(e|ar)?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (h:m:_)):_) -> do
        hh <- parseInteger h
        mnum <- parseInteger m
        let mden = 10 ^ Text.length m
        Just . Token Duration $ minutesFromHourMixedFraction hh mnum mden
      _ -> Nothing
  }

ruleIntegerAndAnHalfHours :: Rule
ruleIntegerAndAnHalfHours = Rule
  { name = "<integer> and an half hours"
  , pattern =
    [ Predicate isNatural
    , regex "och (en )?halv timme?"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:_) ->
        Just . Token Duration . duration TG.Minute $ 30 + 60 * floor v
      _ -> Nothing
  }

ruleAUnitofduration :: Rule
ruleAUnitofduration = Rule
  { name = "a <unit-of-duration>"
  , pattern =
    [ regex "en|ett?"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) ->
        Just . Token Duration $ duration grain 1
      _ -> Nothing
  }

ruleAboutDuration :: Rule
ruleAboutDuration = Rule
  { name = "about <duration>"
  , pattern =
    [ regex "(omkring|cirka|ca\\.?|c:a|runt|ungefär)"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleExactlyDuration :: Rule
ruleExactlyDuration = Rule
  { name = "exactly <duration>"
  , pattern =
    [ regex "(precis|exakt)"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Just token
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleAUnitofduration
  , ruleAboutDuration
  , ruleExactlyDuration
  , ruleHalfAnHour
  , ruleIntegerAndAnHalfHours
  , ruleIntegerMoreUnitofduration
  , ruleNumeralnumberHours
  ]
