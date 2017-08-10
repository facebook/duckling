-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Rules
  ( allRules
  , rulesFor
  ) where

import Data.HashSet (HashSet)
import Prelude
import qualified Data.HashSet as HashSet

import Duckling.Dimensions
import Duckling.Dimensions.Types
import Duckling.Lang
import qualified Duckling.Rules.AR as ARRules
import qualified Duckling.Rules.Common as CommonRules
import qualified Duckling.Rules.BG as BGRules
import qualified Duckling.Rules.CS as CSRules
import qualified Duckling.Rules.DA as DARules
import qualified Duckling.Rules.DE as DERules
import qualified Duckling.Rules.EN as ENRules
import qualified Duckling.Rules.ES as ESRules
import qualified Duckling.Rules.ET as ETRules
import qualified Duckling.Rules.FR as FRRules
import qualified Duckling.Rules.GA as GARules
import qualified Duckling.Rules.HE as HERules
import qualified Duckling.Rules.HR as HRRules
import qualified Duckling.Rules.HU as HURules
import qualified Duckling.Rules.ID as IDRules
import qualified Duckling.Rules.IT as ITRules
import qualified Duckling.Rules.JA as JARules
import qualified Duckling.Rules.KO as KORules
import qualified Duckling.Rules.MY as MYRules
import qualified Duckling.Rules.NB as NBRules
import qualified Duckling.Rules.NL as NLRules
import qualified Duckling.Rules.PL as PLRules
import qualified Duckling.Rules.PT as PTRules
import qualified Duckling.Rules.RO as RORules
import qualified Duckling.Rules.RU as RURules
import qualified Duckling.Rules.SV as SVRules
import qualified Duckling.Rules.TR as TRRules
import qualified Duckling.Rules.UK as UKRules
import qualified Duckling.Rules.VI as VIRules
import qualified Duckling.Rules.ZH as ZHRules
import Duckling.Types

-- | Returns the minimal set of rules required for `targets`.
rulesFor :: Lang -> HashSet (Some Dimension) -> [Rule]
rulesFor lang targets
  | HashSet.null targets = allRules lang
  | otherwise = [ rules | dims <- HashSet.toList $ explicitDimensions targets
                        , rules <- rulesFor' lang dims ]

-- | Returns all the rules for `lang`.
-- We can't really use `allDimensions` as-is, since `TimeGrain` is not present.
allRules :: Lang -> [Rule]
allRules lang = concatMap (rulesFor' lang) . HashSet.toList .
  explicitDimensions . HashSet.fromList $ allDimensions lang

rulesFor' :: Lang -> Some Dimension -> [Rule]
rulesFor' lang dim = CommonRules.rules dim ++ langRules lang dim

langRules :: Lang -> Some Dimension -> [Rule]
langRules AR = ARRules.rules
langRules BG = BGRules.rules
langRules CS = CSRules.rules
langRules DA = DARules.rules
langRules DE = DERules.rules
langRules EN = ENRules.rules
langRules ES = ESRules.rules
langRules ET = ETRules.rules
langRules FR = FRRules.rules
langRules GA = GARules.rules
langRules HE = HERules.rules
langRules HR = HRRules.rules
langRules HU = HURules.rules
langRules ID = IDRules.rules
langRules IT = ITRules.rules
langRules JA = JARules.rules
langRules KO = KORules.rules
langRules MY = MYRules.rules
langRules NB = NBRules.rules
langRules NL = NLRules.rules
langRules PL = PLRules.rules
langRules PT = PTRules.rules
langRules RO = RORules.rules
langRules RU = RURules.rules
langRules SV = SVRules.rules
langRules TR = TRRules.rules
langRules UK = UKRules.rules
langRules VI = VIRules.rules
langRules ZH = ZHRules.rules
