-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
module Duckling.Ranking.Extraction
  ( extractFeatures
  ) where

import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import qualified Data.Text as Text
import Prelude
import TextShow (showt)

import Duckling.Dimensions.Types
import Duckling.Duration.Types (DurationData (DurationData))
import qualified Duckling.Duration.Types as TDuration
import Duckling.Ranking.Types
import Duckling.Time.Types (TimeData (TimeData))
import qualified Duckling.Time.Types as TTime
import Duckling.Types


-- | Feature extraction
-- | Features:
-- | 1) Concatenation of the names of the rules involved in parsing `Node`
-- | 2) Concatenation of the grains for time-like dimensions
extractFeatures :: Node -> BagOfFeatures
extractFeatures node =
  HashMap.fromList $ (featRules, 1) : [ (featGrain, 1) | not (null grains) ]
  where
    featRules = Text.concat $ mapMaybe rule (children node)
    grains = mapMaybe (\x ->
      case token x of
        Token Duration DurationData{TDuration.grain = g} -> Just $ showt g
        Token Time TimeData{TTime.timeGrain = g} -> Just $ showt g
        Token TimeGrain g -> Just $ showt g
        _ -> Nothing
      ) $ children node
    featGrain = Text.concat grains
