-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.AR.Rules
  ( rules
  ) where

import Data.String
import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Types
import Duckling.Regex.Types
import Duckling.Volume.Helpers
import Duckling.Numeral.Helpers (isPositive)
import qualified Duckling.Volume.Types as TVolume
import qualified Duckling.Numeral.Types as TNumeral

volumes :: [(Text, String, TVolume.Unit)]
volumes = [ ("<latent vol> ml"    , "مي?لي?( ?لي?تي?ر)?"  , TVolume.Millilitre)
          , ("<vol> hectoliters"  , "(هي?كتو ?لي?تر)"     , TVolume.Hectolitre)
          , ("<vol> liters"       , "لي?تي?ر(ات)?"        , TVolume.Litre)
          , ("<latent vol> gallon", "[جغق]الون(ين|ان|ات)?", TVolume.Gallon)
          ]

rulesVolumes :: [Rule]
rulesVolumes = map go volumes
  where
    go :: (Text, String, TVolume.Unit) -> Rule
    go (name, regexPattern, u) = Rule
      { name = name
      , pattern =
        [ regex regexPattern
        ]
      , prod = \_ -> Just . Token Volume $ unitOnly u
      }

ruleQuarterLiter :: Rule
ruleQuarterLiter = Rule
  { name = "quarter liter"
  , pattern =
    [ regex "ربع لي?تي?ر"
    ]
  , prod = \_ -> Just . Token Volume . withUnit TVolume.Litre $ valueOnly 0.25
  }

ruleHalfLiter :: Rule
ruleHalfLiter = Rule
  { name = "half liter"
  , pattern =
    [ regex "نصف? لي?تي?ر"
    ]
  , prod = \_ -> Just . Token Volume . withUnit TVolume.Litre $ valueOnly 0.5
  }

ruleLiterAndQuarter :: Rule
ruleLiterAndQuarter = Rule
  { name = "liter and quarter"
  , pattern =
    [ regex "لي?تي?ر و ?ربع"
    ]
  , prod = \_ -> Just . Token Volume . withUnit TVolume.Litre $ valueOnly 1.25
  }

ruleLiterAndHalf :: Rule
ruleLiterAndHalf = Rule
  { name = "liter and half"
  , pattern =
    [ regex "لي?تي?ر و ?نصف?"
    ]
  , prod = \_ -> Just . Token Volume . withUnit TVolume.Litre $ valueOnly 1.5
  }

ruleTwoLiters :: Rule
ruleTwoLiters = Rule
  { name = "two liters"
  , pattern =
    [ regex "لي?تي?ر(ان|ين)"
    ]
  , prod = \_ -> Just . Token Volume . withUnit TVolume.Litre $ valueOnly 2
  }

rules :: [Rule]
rules = [ ruleHalfLiter
        , ruleQuarterLiter
        , ruleTwoLiters
        , ruleLiterAndHalf
        , ruleLiterAndQuarter
        ]
        ++ rulesVolumes
