-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.KO.Rules
  ( rules ) where

import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Types
import Duckling.Volume.Helpers
import qualified Duckling.Volume.Types as TVolume

ruleLatentVolMl :: Rule
ruleLatentVolMl = Rule
  { name = "<latent vol> ml"
  , pattern =
    [ dimension Volume
    , regex "ml|(\xbc00|\xbbf8)\xb9ac\xb9ac\xd130"
    ]
  , prod = \tokens -> case tokens of
      (Token Volume vd:_) ->
        Just . Token Volume $ withUnit TVolume.Millilitre vd
      _ -> Nothing
  }

ruleVolHectoliters :: Rule
ruleVolHectoliters = Rule
  { name = "<vol> hectoliters"
  , pattern =
    [ dimension Volume
    , regex "(\xd575|\xd5e5)\xd1a0\xb9ac\xd130"
    ]
  , prod = \tokens -> case tokens of
      (Token Volume vd:_) ->
        Just . Token Volume $ withUnit TVolume.Hectolitre vd
      _ -> Nothing
  }

ruleVolLiters :: Rule
ruleVolLiters = Rule
  { name = "<vol> liters"
  , pattern =
    [ dimension Volume
    , regex "l|\xb9ac\xd130"
    ]
  , prod = \tokens -> case tokens of
      (Token Volume vd:_) ->
        Just . Token Volume $ withUnit TVolume.Litre vd
      _ -> Nothing
  }

ruleLatentVolGallon :: Rule
ruleLatentVolGallon = Rule
  { name = "<latent vol> gallon"
  , pattern =
    [ dimension Volume
    , regex "gal(l?ons?)?|\xac24(\xb7f0|\xb860)"
    ]
  , prod = \tokens -> case tokens of
      (Token Volume vd:_) ->
        Just . Token Volume $ withUnit TVolume.Gallon vd
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleLatentVolGallon
  , ruleLatentVolMl
  , ruleVolHectoliters
  , ruleVolLiters
  ]
