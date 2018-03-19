-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.AR.Rules
  ( rules
  ) where

import Data.String
import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Types
import Duckling.Volume.Helpers
import qualified Duckling.Volume.Types as TVolume

ruleHalfLiter :: Rule
ruleHalfLiter = Rule
  { name = "half liter"
  , pattern =
    [ regex "نصف? لي?تي?ر"
    ]
  , prod = \_ -> Just . Token Volume . withUnit TVolume.Litre $ volume 0.5
  }

ruleQuartorOfLiter :: Rule
ruleQuartorOfLiter = Rule
  { name = "quartor of liter"
  , pattern =
    [ regex "ربع لي?تي?ر"
    ]
  , prod = \_ -> Just . Token Volume . withUnit TVolume.Litre $ volume 0.25
  }

ruleLiterAndHalf :: Rule
ruleLiterAndHalf = Rule
  { name = "liter and half"
  , pattern =
    [ regex "لي?تي?ر و ?نصف?"
    ]
  , prod = \_ -> Just . Token Volume . withUnit TVolume.Litre $ volume 1.5
  }

ruleTwoLiters :: Rule
ruleTwoLiters = Rule
  { name = "two liters"
  , pattern =
    [ regex "لي?تي?ر(ان|ين)"
    ]
  , prod = \_ -> Just . Token Volume . withUnit TVolume.Litre $ volume 2
  }

ruleLiterAndQuartor :: Rule
ruleLiterAndQuartor = Rule
  { name = "liter and quartor"
  , pattern =
    [ regex "لي?تي?ر و ?ربع"
    ]
  , prod = \_ -> Just . Token Volume . withUnit TVolume.Litre $ volume 1.25
  }

volumes :: [(Text, String, TVolume.Unit)]
volumes =
  [ ("<latent vol> ml"    , "مي?لي?( ?لي?تي?ر)?"  , TVolume.Millilitre)
  , ("<vol> hectoliters"  , "هي?كتو ?لي?تر"       , TVolume.Hectolitre)
  , ("<vol> liters"       , "لي?تي?ر(ات)?"        , TVolume.Litre)
  , ("<latent vol> gallon", "[جغق]الون(ين|ان|ات)?", TVolume.Gallon)
  ]

ruleVolumes :: [Rule]
ruleVolumes = map go volumes
  where
    go :: (Text, String, TVolume.Unit) -> Rule
    go (name, regexPattern, u) = Rule
      { name = name
      , pattern = [ dimension Volume, regex regexPattern ]
      , prod = \tokens -> case tokens of
          (Token Volume vd:_) -> Just . Token Volume $ withUnit u vd
          _ -> Nothing
      }

rules :: [Rule]
rules =
  [ ruleHalfLiter
  , ruleTwoLiters
  , ruleQuartorOfLiter
  , ruleLiterAndHalf
  , ruleLiterAndQuartor
  ]
  ++ ruleVolumes
