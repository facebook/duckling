-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


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
import Duckling.Locale
import Duckling.Types
import qualified Duckling.Rules.AR as ARRules
import qualified Duckling.Rules.Common as CommonRules
import qualified Duckling.Rules.BG as BGRules
import qualified Duckling.Rules.BN as BNRules
import qualified Duckling.Rules.CS as CSRules
import qualified Duckling.Rules.DA as DARules
import qualified Duckling.Rules.DE as DERules
import qualified Duckling.Rules.EL as ELRules
import qualified Duckling.Rules.EN as ENRules
import qualified Duckling.Rules.ES as ESRules
import qualified Duckling.Rules.ET as ETRules
import qualified Duckling.Rules.FI as FIRules
import qualified Duckling.Rules.FR as FRRules
import qualified Duckling.Rules.GA as GARules
import qualified Duckling.Rules.HE as HERules
import qualified Duckling.Rules.HI as HIRules
import qualified Duckling.Rules.HR as HRRules
import qualified Duckling.Rules.HU as HURules
import qualified Duckling.Rules.ID as IDRules
import qualified Duckling.Rules.IS as ISRules
import qualified Duckling.Rules.IT as ITRules
import qualified Duckling.Rules.JA as JARules
import qualified Duckling.Rules.KA as KARules
import qualified Duckling.Rules.KM as KMRules
import qualified Duckling.Rules.KN as KNRules
import qualified Duckling.Rules.KO as KORules
import qualified Duckling.Rules.LO as LORules
import qualified Duckling.Rules.ML as MLRules
import qualified Duckling.Rules.MN as MNRules
import qualified Duckling.Rules.MY as MYRules
import qualified Duckling.Rules.NB as NBRules
import qualified Duckling.Rules.NE as NERules
import qualified Duckling.Rules.NL as NLRules
import qualified Duckling.Rules.PL as PLRules
import qualified Duckling.Rules.PT as PTRules
import qualified Duckling.Rules.RO as RORules
import qualified Duckling.Rules.RU as RURules
import qualified Duckling.Rules.SV as SVRules
import qualified Duckling.Rules.SW as SWRules
import qualified Duckling.Rules.TA as TARules
import qualified Duckling.Rules.TR as TRRules
import qualified Duckling.Rules.UK as UKRules
import qualified Duckling.Rules.VI as VIRules
import qualified Duckling.Rules.ZH as ZHRules

-- | Returns the minimal set of rules required for `targets`.
rulesFor :: Locale -> HashSet (Some Dimension) -> [Rule]
rulesFor locale targets
  | HashSet.null targets = allRules locale
  | otherwise = [ rules | dims <- HashSet.toList $ explicitDimensions targets
                        , rules <- rulesFor' locale dims ]

-- | Returns all the rules for the provided locale.
-- We can't really use `allDimensions` as-is, since `TimeGrain` is not present.
allRules :: Locale -> [Rule]
allRules locale@(Locale lang _) = concatMap (rulesFor' locale) . HashSet.toList
  . explicitDimensions . HashSet.fromList $ allDimensions lang

rulesFor' :: Locale -> Some Dimension -> [Rule]
rulesFor' (Locale lang (Just region)) dim =
  CommonRules.rules dim ++ langRules lang dim ++ localeRules lang region dim
rulesFor' (Locale lang Nothing) dim =
  CommonRules.rules dim ++ defaultRules lang dim

-- | Default rules when no locale, for backward compatibility.
defaultRules :: Lang -> Some Dimension -> [Rule]
defaultRules AR = ARRules.defaultRules
defaultRules BG = BGRules.defaultRules
defaultRules BN = BNRules.defaultRules
defaultRules CS = CSRules.defaultRules
defaultRules DA = DARules.defaultRules
defaultRules DE = DERules.defaultRules
defaultRules EL = ELRules.defaultRules
defaultRules EN = ENRules.defaultRules
defaultRules ES = ESRules.defaultRules
defaultRules ET = ETRules.defaultRules
defaultRules FI = FIRules.defaultRules
defaultRules FR = FRRules.defaultRules
defaultRules GA = GARules.defaultRules
defaultRules HE = HERules.defaultRules
defaultRules HI = HIRules.defaultRules
defaultRules HR = HRRules.defaultRules
defaultRules HU = HURules.defaultRules
defaultRules ID = IDRules.defaultRules
defaultRules IS = ISRules.defaultRules
defaultRules IT = ITRules.defaultRules
defaultRules JA = JARules.defaultRules
defaultRules KA = KARules.defaultRules
defaultRules KM = KMRules.defaultRules
defaultRules KN = KNRules.defaultRules
defaultRules KO = KORules.defaultRules
defaultRules LO = LORules.defaultRules
defaultRules ML = MLRules.defaultRules
defaultRules MN = MNRules.defaultRules
defaultRules MY = MYRules.defaultRules
defaultRules NB = NBRules.defaultRules
defaultRules NE = NERules.defaultRules
defaultRules NL = NLRules.defaultRules
defaultRules PL = PLRules.defaultRules
defaultRules PT = PTRules.defaultRules
defaultRules RO = RORules.defaultRules
defaultRules RU = RURules.defaultRules
defaultRules SV = SVRules.defaultRules
defaultRules SW = SWRules.defaultRules
defaultRules TA = TARules.defaultRules
defaultRules TR = TRRules.defaultRules
defaultRules UK = UKRules.defaultRules
defaultRules VI = VIRules.defaultRules
defaultRules ZH = ZHRules.defaultRules

localeRules :: Lang -> Region -> Some Dimension -> [Rule]
localeRules AR = ARRules.localeRules
localeRules BG = BGRules.localeRules
localeRules BN = BNRules.localeRules
localeRules CS = CSRules.localeRules
localeRules DA = DARules.localeRules
localeRules DE = DERules.localeRules
localeRules EL = ELRules.localeRules
localeRules EN = ENRules.localeRules
localeRules ES = ESRules.localeRules
localeRules ET = ETRules.localeRules
localeRules FI = FIRules.localeRules
localeRules FR = FRRules.localeRules
localeRules GA = GARules.localeRules
localeRules HE = HERules.localeRules
localeRules HI = HIRules.localeRules
localeRules HR = HRRules.localeRules
localeRules HU = HURules.localeRules
localeRules ID = IDRules.localeRules
localeRules IS = ISRules.localeRules
localeRules IT = ITRules.localeRules
localeRules JA = JARules.localeRules
localeRules KA = KARules.localeRules
localeRules KM = KMRules.localeRules
localeRules KN = KNRules.localeRules
localeRules KO = KORules.localeRules
localeRules LO = LORules.localeRules
localeRules ML = MLRules.localeRules
localeRules MN = MNRules.localeRules
localeRules MY = MYRules.localeRules
localeRules NB = NBRules.localeRules
localeRules NE = NERules.localeRules
localeRules NL = NLRules.localeRules
localeRules PL = PLRules.localeRules
localeRules PT = PTRules.localeRules
localeRules RO = RORules.localeRules
localeRules RU = RURules.localeRules
localeRules SV = SVRules.localeRules
localeRules SW = SWRules.localeRules
localeRules TA = TARules.localeRules
localeRules TR = TRRules.localeRules
localeRules UK = UKRules.localeRules
localeRules VI = VIRules.localeRules
localeRules ZH = ZHRules.localeRules

langRules :: Lang -> Some Dimension -> [Rule]
langRules AR = ARRules.langRules
langRules BG = BGRules.langRules
langRules BN = BNRules.langRules
langRules CS = CSRules.langRules
langRules DA = DARules.langRules
langRules DE = DERules.langRules
langRules EL = ELRules.langRules
langRules EN = ENRules.langRules
langRules ES = ESRules.langRules
langRules ET = ETRules.langRules
langRules FI = FIRules.langRules
langRules FR = FRRules.langRules
langRules GA = GARules.langRules
langRules HE = HERules.langRules
langRules HI = HIRules.langRules
langRules KN = KNRules.langRules
langRules HR = HRRules.langRules
langRules HU = HURules.langRules
langRules ID = IDRules.langRules
langRules IS = ISRules.langRules
langRules IT = ITRules.langRules
langRules JA = JARules.langRules
langRules KA = KARules.langRules
langRules KM = KMRules.langRules
langRules KO = KORules.langRules
langRules LO = LORules.langRules
langRules ML = MLRules.langRules
langRules MN = MNRules.langRules
langRules MY = MYRules.langRules
langRules NB = NBRules.langRules
langRules NE = NERules.langRules
langRules NL = NLRules.langRules
langRules PL = PLRules.langRules
langRules PT = PTRules.langRules
langRules RO = RORules.langRules
langRules RU = RURules.langRules
langRules SV = SVRules.langRules
langRules SW = SWRules.langRules
langRules TA = TARules.langRules
langRules TR = TRRules.langRules
langRules UK = UKRules.langRules
langRules VI = VIRules.langRules
langRules ZH = ZHRules.langRules
