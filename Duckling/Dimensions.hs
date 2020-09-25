-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Dimensions
  ( allDimensions
  , explicitDimensions
  ) where

import Data.HashSet (HashSet)
import Prelude
import qualified Data.HashSet as HashSet

import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Types
import qualified Duckling.Dimensions.Common as CommonDimensions
import qualified Duckling.Dimensions.AF as AFDimensions
import qualified Duckling.Dimensions.AR as ARDimensions
import qualified Duckling.Dimensions.BG as BGDimensions
import qualified Duckling.Dimensions.BN as BNDimensions
import qualified Duckling.Dimensions.CS as CSDimensions
import qualified Duckling.Dimensions.DA as DADimensions
import qualified Duckling.Dimensions.DE as DEDimensions
import qualified Duckling.Dimensions.EL as ELDimensions
import qualified Duckling.Dimensions.EN as ENDimensions
import qualified Duckling.Dimensions.ES as ESDimensions
import qualified Duckling.Dimensions.ET as ETDimensions
import qualified Duckling.Dimensions.FI as FIDimensions
import qualified Duckling.Dimensions.FR as FRDimensions
import qualified Duckling.Dimensions.GA as GADimensions
import qualified Duckling.Dimensions.HE as HEDimensions
import qualified Duckling.Dimensions.HI as HIDimensions
import qualified Duckling.Dimensions.HR as HRDimensions
import qualified Duckling.Dimensions.HU as HUDimensions
import qualified Duckling.Dimensions.ID as IDDimensions
import qualified Duckling.Dimensions.IS as ISDimensions
import qualified Duckling.Dimensions.IT as ITDimensions
import qualified Duckling.Dimensions.JA as JADimensions
import qualified Duckling.Dimensions.KA as KADimensions
import qualified Duckling.Dimensions.KM as KMDimensions
import qualified Duckling.Dimensions.KN as KNDimensions
import qualified Duckling.Dimensions.KO as KODimensions
import qualified Duckling.Dimensions.LO as LODimensions
import qualified Duckling.Dimensions.ML as MLDimensions
import qualified Duckling.Dimensions.MN as MNDimensions
import qualified Duckling.Dimensions.MY as MYDimensions
import qualified Duckling.Dimensions.NB as NBDimensions
import qualified Duckling.Dimensions.NE as NEDimensions
import qualified Duckling.Dimensions.NL as NLDimensions
import qualified Duckling.Dimensions.PL as PLDimensions
import qualified Duckling.Dimensions.PT as PTDimensions
import qualified Duckling.Dimensions.RO as RODimensions
import qualified Duckling.Dimensions.RU as RUDimensions
import qualified Duckling.Dimensions.SK as SKDimensions
import qualified Duckling.Dimensions.SV as SVDimensions
import qualified Duckling.Dimensions.SW as SWDimensions
import qualified Duckling.Dimensions.TA as TADimensions
import qualified Duckling.Dimensions.TH as THDimensions
import qualified Duckling.Dimensions.TR as TRDimensions
import qualified Duckling.Dimensions.UK as UKDimensions
import qualified Duckling.Dimensions.VI as VIDimensions
import qualified Duckling.Dimensions.ZH as ZHDimensions


allDimensions :: Lang -> [Seal Dimension]
allDimensions lang = CommonDimensions.allDimensions ++ langDimensions lang

-- | Augments `targets` with all dependent dimensions.
explicitDimensions :: HashSet (Seal Dimension) -> HashSet (Seal Dimension)
explicitDimensions targets = HashSet.union targets deps
  where
    deps = HashSet.unions . map dependents $ HashSet.toList targets

-- | Ordinal depends on Numeral for JA, KO, and ZH.
dependents :: Seal Dimension -> HashSet (Seal Dimension)
dependents (Seal CreditCardNumber) = HashSet.empty
dependents (Seal Distance) = HashSet.singleton (Seal Numeral)
dependents (Seal Duration) = HashSet.fromList [Seal Numeral, Seal TimeGrain]
dependents (Seal Numeral) = HashSet.empty
dependents (Seal Email) = HashSet.empty
dependents (Seal AmountOfMoney) = HashSet.singleton (Seal Numeral)
dependents (Seal Ordinal) = HashSet.singleton (Seal Numeral)
dependents (Seal PhoneNumber) = HashSet.empty
dependents (Seal Quantity) = HashSet.singleton (Seal Numeral)
dependents (Seal RegexMatch) = HashSet.empty
dependents (Seal Temperature) = HashSet.singleton (Seal Numeral)
dependents (Seal Time) =
  HashSet.fromList [Seal Numeral, Seal Duration, Seal Ordinal, Seal TimeGrain]
dependents (Seal TimeGrain) = HashSet.empty
dependents (Seal Url) = HashSet.empty
dependents (Seal Volume) = HashSet.singleton (Seal Numeral)
dependents (Seal (CustomDimension dim)) = dimDependents dim

langDimensions :: Lang -> [Seal Dimension]
langDimensions AF = AFDimensions.allDimensions
langDimensions AR = ARDimensions.allDimensions
langDimensions BG = BGDimensions.allDimensions
langDimensions BN = BNDimensions.allDimensions
langDimensions CS = CSDimensions.allDimensions
langDimensions DA = DADimensions.allDimensions
langDimensions DE = DEDimensions.allDimensions
langDimensions EL = ELDimensions.allDimensions
langDimensions EN = ENDimensions.allDimensions
langDimensions ES = ESDimensions.allDimensions
langDimensions ET = ETDimensions.allDimensions
langDimensions FI = FIDimensions.allDimensions
langDimensions FR = FRDimensions.allDimensions
langDimensions GA = GADimensions.allDimensions
langDimensions HE = HEDimensions.allDimensions
langDimensions HI = HIDimensions.allDimensions
langDimensions HR = HRDimensions.allDimensions
langDimensions HU = HUDimensions.allDimensions
langDimensions ID = IDDimensions.allDimensions
langDimensions IS = ISDimensions.allDimensions
langDimensions IT = ITDimensions.allDimensions
langDimensions JA = JADimensions.allDimensions
langDimensions KA = KADimensions.allDimensions
langDimensions KM = KMDimensions.allDimensions
langDimensions KN = KNDimensions.allDimensions
langDimensions KO = KODimensions.allDimensions
langDimensions LO = LODimensions.allDimensions
langDimensions ML = MLDimensions.allDimensions
langDimensions MN = MNDimensions.allDimensions
langDimensions MY = MYDimensions.allDimensions
langDimensions NB = NBDimensions.allDimensions
langDimensions NE = NEDimensions.allDimensions
langDimensions NL = NLDimensions.allDimensions
langDimensions PL = PLDimensions.allDimensions
langDimensions PT = PTDimensions.allDimensions
langDimensions RO = RODimensions.allDimensions
langDimensions RU = RUDimensions.allDimensions
langDimensions SK = SKDimensions.allDimensions
langDimensions SV = SVDimensions.allDimensions
langDimensions SW = SWDimensions.allDimensions
langDimensions TA = TADimensions.allDimensions
langDimensions TH = THDimensions.allDimensions
langDimensions TR = TRDimensions.allDimensions
langDimensions UK = UKDimensions.allDimensions
langDimensions VI = VIDimensions.allDimensions
langDimensions ZH = ZHDimensions.allDimensions
