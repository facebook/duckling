-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Dimensions
  ( allDimensions
  , explicitDimensions
  ) where
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Prelude

import Duckling.Dimensions.Types
import qualified Duckling.Dimensions.Common as CommonDimensions
import qualified Duckling.Dimensions.AR as ARDimensions
import qualified Duckling.Dimensions.DA as DADimensions
import qualified Duckling.Dimensions.DE as DEDimensions
import qualified Duckling.Dimensions.EN as ENDimensions
import qualified Duckling.Dimensions.ES as ESDimensions
import qualified Duckling.Dimensions.ET as ETDimensions
import qualified Duckling.Dimensions.FR as FRDimensions
import qualified Duckling.Dimensions.GA as GADimensions
import qualified Duckling.Dimensions.ID as IDDimensions
import qualified Duckling.Dimensions.IT as ITDimensions
import qualified Duckling.Dimensions.JA as JADimensions
import qualified Duckling.Dimensions.KO as KODimensions
import qualified Duckling.Dimensions.MY as MYDimensions
import qualified Duckling.Dimensions.NB as NBDimensions
import qualified Duckling.Dimensions.NL as NLDimensions
import qualified Duckling.Dimensions.PL as PLDimensions
import qualified Duckling.Dimensions.PT as PTDimensions
import qualified Duckling.Dimensions.RO as RODimensions
import qualified Duckling.Dimensions.RU as RUDimensions
import qualified Duckling.Dimensions.SV as SVDimensions
import qualified Duckling.Dimensions.TR as TRDimensions
import qualified Duckling.Dimensions.UK as UKDimensions
import qualified Duckling.Dimensions.VI as VIDimensions
import qualified Duckling.Dimensions.ZH as ZHDimensions
import Duckling.Lang

allDimensions :: Lang -> [Some Dimension]
allDimensions lang = CommonDimensions.allDimensions ++ langDimensions lang

-- | Augments `targets` with all dependent dimensions.
explicitDimensions :: HashSet (Some Dimension) -> HashSet (Some Dimension)
explicitDimensions targets = HashSet.union targets deps
  where
    deps = HashSet.unions . map dependents $ HashSet.toList targets

-- | Ordinal depends on DNumber for JA, KO, and ZH.
dependents :: Some Dimension -> HashSet (Some Dimension)
dependents (Some Distance) = HashSet.singleton (Some DNumber)
dependents (Some Duration) = HashSet.fromList [Some DNumber, Some TimeGrain]
dependents (Some DNumber) = HashSet.empty
dependents (Some Email) = HashSet.empty
dependents (Some Finance) = HashSet.singleton (Some DNumber)
dependents (Some Ordinal) = HashSet.singleton (Some DNumber)
dependents (Some PhoneNumber) = HashSet.empty
dependents (Some Quantity) = HashSet.singleton (Some DNumber)
dependents (Some RegexMatch) = HashSet.empty
dependents (Some Temperature) = HashSet.singleton (Some DNumber)
dependents (Some Time) =
  HashSet.fromList [Some DNumber, Some Duration, Some Ordinal, Some TimeGrain]
dependents (Some TimeGrain) = HashSet.empty
dependents (Some Url) = HashSet.empty
dependents (Some Volume) = HashSet.singleton (Some DNumber)

langDimensions :: Lang -> [Some Dimension]
langDimensions AR = ARDimensions.allDimensions
langDimensions DA = DADimensions.allDimensions
langDimensions DE = DEDimensions.allDimensions
langDimensions EN = ENDimensions.allDimensions
langDimensions ES = ESDimensions.allDimensions
langDimensions ET = ETDimensions.allDimensions
langDimensions FR = FRDimensions.allDimensions
langDimensions GA = GADimensions.allDimensions
langDimensions ID = IDDimensions.allDimensions
langDimensions IT = ITDimensions.allDimensions
langDimensions JA = JADimensions.allDimensions
langDimensions KO = KODimensions.allDimensions
langDimensions MY = MYDimensions.allDimensions
langDimensions NB = NBDimensions.allDimensions
langDimensions NL = NLDimensions.allDimensions
langDimensions PL = PLDimensions.allDimensions
langDimensions PT = PTDimensions.allDimensions
langDimensions RO = RODimensions.allDimensions
langDimensions RU = RUDimensions.allDimensions
langDimensions SV = SVDimensions.allDimensions
langDimensions TR = TRDimensions.allDimensions
langDimensions UK = UKDimensions.allDimensions
langDimensions VI = VIDimensions.allDimensions
langDimensions ZH = ZHDimensions.allDimensions
