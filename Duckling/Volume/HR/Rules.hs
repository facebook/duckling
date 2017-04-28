-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.HR.Rules
  ( rules ) where

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
    , regex "m(l|ililita?ra?)"
    ]
  , prod = \tokens -> case tokens of
      (Token Volume vd:_) ->
        Just . Token Volume $ withUnit TVolume.Millilitre vd
      _ -> Nothing
  }

ruleVolHektolitar :: Rule
ruleVolHektolitar = Rule
  { name = "<vol> hektolitar"
  , pattern =
    [ dimension Volume
    , regex "hektolita?ra?"
    ]
  , prod = \tokens -> case tokens of
      (Token Volume vd:_) ->
        Just . Token Volume $ withUnit TVolume.Hectolitre vd
      _ -> Nothing
  }

ruleVolLitra :: Rule
ruleVolLitra = Rule
  { name = "<vol> litra"
  , pattern =
    [ dimension Volume
    , regex "l(it(a)?r(a|e)?)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Volume vd:_) ->
        Just . Token Volume $ withUnit TVolume.Litre vd
      _ -> Nothing
  }

rulePolaLitre :: Rule
rulePolaLitre = Rule
  { name = "pola litre"
  , pattern =
    [ regex "pola litre"
    ]
  , prod = \_ -> Just . Token Volume . withUnit TVolume.Litre $ volume 0.5
  }

ruleLatentVolGalon :: Rule
ruleLatentVolGalon = Rule
  { name = "<latent vol> galon"
  , pattern =
    [ dimension Volume
    , regex "gal(ona?)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Volume vd:_) ->
        Just . Token Volume $ withUnit TVolume.Gallon vd
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleLatentVolGalon
  , ruleLatentVolMl
  , rulePolaLitre
  , ruleVolHektolitar
  , ruleVolLitra
  ]
