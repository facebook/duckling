-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


module Duckling.Volume.Helpers
  ( volume
  , withUnit
  ) where

import Prelude

import Duckling.Volume.Types (VolumeData(..))
import qualified Duckling.Volume.Types as TVolume

-- -----------------------------------------------------------------
-- Patterns

-- -----------------------------------------------------------------
-- Production

volume :: Double -> VolumeData
volume x = VolumeData {TVolume.value = x, TVolume.unit = Nothing}

withUnit :: TVolume.Unit -> VolumeData -> VolumeData
withUnit value vd = vd {TVolume.unit = Just value}
