-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.RO.Rules
  ( rules
  ) where

import Data.String
import Prelude

import Duckling.Dimensions.Types
import Duckling.Types
import Duckling.Volume.Helpers
import qualified Duckling.Volume.Types as TVolume

ruleLatentVolMl :: Rule
ruleLatentVolMl = Rule
  { name = "<latent vol> ml"
  , pattern =
    [ dimension Volume
    , regex "(de )?m(ililitr[ui]|l)"
    ]
  , prod = \case
      (Token Volume vd:_) ->
        Just . Token Volume $ withUnit TVolume.Millilitre vd
      _ -> Nothing
  }

ruleVolHectoliters :: Rule
ruleVolHectoliters = Rule
  { name = "<vol> hectoliters"
  , pattern =
    [ dimension Volume
    , regex "(de )?hectolitr[ui]"
    ]
  , prod = \case
      (Token Volume vd:_) ->
        Just . Token Volume $ withUnit TVolume.Hectolitre vd
      _ -> Nothing
  }

ruleVolLiters :: Rule
ruleVolLiters = Rule
  { name = "<vol> liters"
  , pattern =
    [ dimension Volume
    , regex "(de )?l(itr[ui])?"
    ]
  , prod = \case
      (Token Volume vd:_) ->
        Just . Token Volume $ withUnit TVolume.Litre vd
      _ -> Nothing
  }

ruleHalfLiter :: Rule
ruleHalfLiter = Rule
  { name = "half liter"
  , pattern =
    [ regex "jum(a|ă)tate de litr[ui]"
    ]
  , prod = const . Just . Token Volume . withUnit TVolume.Litre $ volume 0.5
  }

ruleLatentVolGalon :: Rule
ruleLatentVolGalon = Rule
  { name = "<latent vol> galon"
  , pattern =
    [ dimension Volume
    , regex "(de )?gal(oane|on)?"
    ]
  , prod = \case
      (Token Volume vd:_) ->
        Just . Token Volume $ withUnit TVolume.Gallon vd
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleHalfLiter
  , ruleLatentVolGalon
  , ruleLatentVolMl
  , ruleVolHectoliters
  , ruleVolLiters
  ]
