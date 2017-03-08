-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.GA.Rules
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
    , regex "m(l\\.?|h?illil(\x00ed|i)t(ea|i)r)"
    ]
  , prod = \tokens -> case tokens of
      (Token Volume vd:_) ->
        Just . Token Volume $ withUnit TVolume.Millilitre vd
      _ -> Nothing
  }

ruleLatentVolKl :: Rule
ruleLatentVolKl = Rule
  { name = "<latent vol> kl"
  , pattern =
    [ dimension Volume
    , regex "(kl\\.?|g?ch?illil(\x00ed|i)t(ea|i)r)"
    ]
  , prod = \tokens -> case tokens of
      (Token Volume vd:_) ->
        Just . Token Volume $ withUnit TVolume.Millilitre vd
      _ -> Nothing
  }

ruleVolHeictiltir :: Rule
ruleVolHeictiltir = Rule
  { name = "<vol> heictilítir"
  , pattern =
    [ dimension Volume
    , regex "heictil(\x00ed|i)t(ea|i)r"
    ]
  , prod = \tokens -> case tokens of
      (Token Volume vd:_) ->
        Just . Token Volume $ withUnit TVolume.Hectolitre vd
      _ -> Nothing
  }

ruleVolLtear :: Rule
ruleVolLtear = Rule
  { name = "<vol> lítear"
  , pattern =
    [ dimension Volume
    , regex "(l(\x00ed|i)t(ea|i)r|l\\.?)"
    ]
  , prod = \tokens -> case tokens of
      (Token Volume vd:_) ->
        Just . Token Volume $ withUnit TVolume.Litre vd
      _ -> Nothing
  }

ruleLatentVolGaln :: Rule
ruleLatentVolGaln = Rule
  { name = "<latent vol> galún"
  , pattern =
    [ dimension Volume
    , regex "n?gh?al(\x00fa|u)i?n"
    ]
  , prod = \tokens -> case tokens of
      (Token Volume vd:_) ->
        Just . Token Volume $ withUnit TVolume.Gallon vd
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleLatentVolGaln
  , ruleLatentVolKl
  , ruleLatentVolMl
  , ruleVolHeictiltir
  , ruleVolLtear
  ]
