-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.AR.Rules
  ( rules ) where

import Prelude
import Data.String

import Duckling.Ordinal.Helpers
import Duckling.Types

ruleOrdinalsTh :: Rule
ruleOrdinalsTh = Rule
  { name = "ordinals 7th"
  , pattern =
    [ regex "(سابع | سابعة | السابع | السابعة)"
    ]
  , prod = \_ -> Just $ ordinal 7
  }

ruleOrdinalsSecond :: Rule
ruleOrdinalsSecond = Rule
  { name = "ordinals second"
  , pattern =
    [ regex "(ثاني|ثانية|الثاني|الثانية)"
    ]
  , prod = \_ -> Just $ ordinal 2
  }

ruleOrdinalsFirst :: Rule
ruleOrdinalsFirst = Rule
  { name = "ordinals first"
  , pattern =
    [ regex "(أول|الأول|أولى|الأولى)"
    ]
  , prod = \_ -> Just $ ordinal 1
  }

ruleOrdinalsFirst5 :: Rule
ruleOrdinalsFirst5 = Rule
  { name = "ordinals first"
  , pattern =
    [ regex "(سادس | سادسة | السادس | السادسة)"
    ]
  , prod = \_ -> Just $ ordinal 6
  }

ruleOrdinalsTh2 :: Rule
ruleOrdinalsTh2 = Rule
  { name = "ordinals 8th"
  , pattern =
    [ regex "(ثامن | ثامنة | الثامن | الثامنة)"
    ]
  , prod = \_ -> Just $ ordinal 8
  }

ruleOrdinalsFirst2 :: Rule
ruleOrdinalsFirst2 = Rule
  { name = "ordinals first"
  , pattern =
    [ regex "(ثالث|ثالثة|الثالث|الثالثة)"
    ]
  , prod = \_ -> Just $ ordinal 3
  }

ruleOrdinalsTh4 :: Rule
ruleOrdinalsTh4 = Rule
  { name = "ordinals 10th"
  , pattern =
    [ regex "(عاشر | عاشرة | العاشر | العاشرة)"
    ]
  , prod = \_ -> Just $ ordinal 10
  }

ruleOrdinalsTh3 :: Rule
ruleOrdinalsTh3 = Rule
  { name = "ordinals 9th"
  , pattern =
    [ regex "(تاسع | تاسعة | التاسع | التاسعة)"
    ]
  , prod = \_ -> Just $ ordinal 9
  }

ruleOrdinalsFirst4 :: Rule
ruleOrdinalsFirst4 = Rule
  { name = "ordinals first"
  , pattern =
    [ regex "(خامس | الخامس | خامسة | الخامسة)"
    ]
  , prod = \_ -> Just $ ordinal 5
  }

ruleOrdinalsFirst3 :: Rule
ruleOrdinalsFirst3 = Rule
  { name = "ordinals first"
  , pattern =
    [ regex "(رابع|رابعة | الرابع|الرابعة)"
    ]
  , prod = \_ -> Just $ ordinal 4
  }

rules :: [Rule]
rules =
  [ ruleOrdinalsFirst
  , ruleOrdinalsFirst2
  , ruleOrdinalsFirst3
  , ruleOrdinalsFirst4
  , ruleOrdinalsFirst5
  , ruleOrdinalsSecond
  , ruleOrdinalsTh
  , ruleOrdinalsTh2
  , ruleOrdinalsTh3
  , ruleOrdinalsTh4
  ]
