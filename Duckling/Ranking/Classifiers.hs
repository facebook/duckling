-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


module Duckling.Ranking.Classifiers
  ( classifiers
  ) where

import Data.Maybe

import Duckling.Locale
import Duckling.Ranking.Types
import qualified Duckling.Ranking.Classifiers.AR_XX as AR_XXClassifiers
import qualified Duckling.Ranking.Classifiers.BG_XX as BG_XXClassifiers
import qualified Duckling.Ranking.Classifiers.BN_XX as BN_XXClassifiers
import qualified Duckling.Ranking.Classifiers.CS_XX as CS_XXClassifiers
import qualified Duckling.Ranking.Classifiers.DA_XX as DA_XXClassifiers
import qualified Duckling.Ranking.Classifiers.DE_XX as DE_XXClassifiers
import qualified Duckling.Ranking.Classifiers.EL_XX as EL_XXClassifiers
import qualified Duckling.Ranking.Classifiers.EN_GB as EN_GBClassifiers
import qualified Duckling.Ranking.Classifiers.EN_US as EN_USClassifiers
import qualified Duckling.Ranking.Classifiers.EN_XX as EN_XXClassifiers
import qualified Duckling.Ranking.Classifiers.ES_XX as ES_XXClassifiers
import qualified Duckling.Ranking.Classifiers.ET_XX as ET_XXClassifiers
import qualified Duckling.Ranking.Classifiers.FI_XX as FI_XXClassifiers
import qualified Duckling.Ranking.Classifiers.FR_XX as FR_XXClassifiers
import qualified Duckling.Ranking.Classifiers.GA_XX as GA_XXClassifiers
import qualified Duckling.Ranking.Classifiers.HE_XX as HE_XXClassifiers
import qualified Duckling.Ranking.Classifiers.HI_XX as HI_XXClassifiers
import qualified Duckling.Ranking.Classifiers.HR_XX as HR_XXClassifiers
import qualified Duckling.Ranking.Classifiers.HU_XX as HU_XXClassifiers
import qualified Duckling.Ranking.Classifiers.ID_XX as ID_XXClassifiers
import qualified Duckling.Ranking.Classifiers.IS_XX as IS_XXClassifiers
import qualified Duckling.Ranking.Classifiers.IT_XX as IT_XXClassifiers
import qualified Duckling.Ranking.Classifiers.JA_XX as JA_XXClassifiers
import qualified Duckling.Ranking.Classifiers.KA_XX as KA_XXClassifiers
import qualified Duckling.Ranking.Classifiers.KM_XX as KM_XXClassifiers
import qualified Duckling.Ranking.Classifiers.KN_XX as KN_XXClassifiers
import qualified Duckling.Ranking.Classifiers.KO_XX as KO_XXClassifiers
import qualified Duckling.Ranking.Classifiers.LO_XX as LO_XXClassifiers
import qualified Duckling.Ranking.Classifiers.ML_XX as ML_XXClassifiers
import qualified Duckling.Ranking.Classifiers.MN_XX as MN_XXClassifiers
import qualified Duckling.Ranking.Classifiers.MY_XX as MY_XXClassifiers
import qualified Duckling.Ranking.Classifiers.NB_XX as NB_XXClassifiers
import qualified Duckling.Ranking.Classifiers.NE_XX as NE_XXClassifiers
import qualified Duckling.Ranking.Classifiers.NL_XX as NL_XXClassifiers
import qualified Duckling.Ranking.Classifiers.PL_XX as PL_XXClassifiers
import qualified Duckling.Ranking.Classifiers.PT_XX as PT_XXClassifiers
import qualified Duckling.Ranking.Classifiers.RO_XX as RO_XXClassifiers
import qualified Duckling.Ranking.Classifiers.RU_XX as RU_XXClassifiers
import qualified Duckling.Ranking.Classifiers.SV_XX as SV_XXClassifiers
import qualified Duckling.Ranking.Classifiers.SW_XX as SW_XXClassifiers
import qualified Duckling.Ranking.Classifiers.TA_XX as TA_XXClassifiers
import qualified Duckling.Ranking.Classifiers.TR_XX as TR_XXClassifiers
import qualified Duckling.Ranking.Classifiers.UK_XX as UK_XXClassifiers
import qualified Duckling.Ranking.Classifiers.VI_XX as VI_XXClassifiers
import qualified Duckling.Ranking.Classifiers.ZH_XX as ZH_XXClassifiers

classifiers :: Locale -> Classifiers
classifiers (Locale AR _) = AR_XXClassifiers.classifiers
classifiers (Locale BG _) = BG_XXClassifiers.classifiers
classifiers (Locale BN _) = BN_XXClassifiers.classifiers
classifiers (Locale CS _) = CS_XXClassifiers.classifiers
classifiers (Locale DA _) = DA_XXClassifiers.classifiers
classifiers (Locale DE _) = DE_XXClassifiers.classifiers
classifiers (Locale EL _) = EL_XXClassifiers.classifiers
classifiers (Locale EN (Just GB)) = EN_GBClassifiers.classifiers
classifiers (Locale EN (Just US)) = EN_USClassifiers.classifiers
classifiers (Locale EN _) = EN_XXClassifiers.classifiers
classifiers (Locale ES _) = ES_XXClassifiers.classifiers
classifiers (Locale ET _) = ET_XXClassifiers.classifiers
classifiers (Locale FI _) = FI_XXClassifiers.classifiers
classifiers (Locale FR _) = FR_XXClassifiers.classifiers
classifiers (Locale GA _) = GA_XXClassifiers.classifiers
classifiers (Locale HE _) = HE_XXClassifiers.classifiers
classifiers (Locale HI _) = HI_XXClassifiers.classifiers
classifiers (Locale HR _) = HR_XXClassifiers.classifiers
classifiers (Locale HU _) = HU_XXClassifiers.classifiers
classifiers (Locale ID _) = ID_XXClassifiers.classifiers
classifiers (Locale IS _) = IS_XXClassifiers.classifiers
classifiers (Locale IT _) = IT_XXClassifiers.classifiers
classifiers (Locale JA _) = JA_XXClassifiers.classifiers
classifiers (Locale KA _) = KA_XXClassifiers.classifiers
classifiers (Locale KM _) = KM_XXClassifiers.classifiers
classifiers (Locale KN _) = KN_XXClassifiers.classifiers
classifiers (Locale KO _) = KO_XXClassifiers.classifiers
classifiers (Locale LO _) = LO_XXClassifiers.classifiers
classifiers (Locale ML _) = ML_XXClassifiers.classifiers
classifiers (Locale MN _) = MN_XXClassifiers.classifiers
classifiers (Locale MY _) = MY_XXClassifiers.classifiers
classifiers (Locale NB _) = NB_XXClassifiers.classifiers
classifiers (Locale NE _) = NE_XXClassifiers.classifiers
classifiers (Locale NL _) = NL_XXClassifiers.classifiers
classifiers (Locale PL _) = PL_XXClassifiers.classifiers
classifiers (Locale PT _) = PT_XXClassifiers.classifiers
classifiers (Locale RO _) = RO_XXClassifiers.classifiers
classifiers (Locale RU _) = RU_XXClassifiers.classifiers
classifiers (Locale SV _) = SV_XXClassifiers.classifiers
classifiers (Locale SW _) = SW_XXClassifiers.classifiers
classifiers (Locale TA _) = TA_XXClassifiers.classifiers
classifiers (Locale TR _) = TR_XXClassifiers.classifiers
classifiers (Locale UK _) = UK_XXClassifiers.classifiers
classifiers (Locale VI _) = VI_XXClassifiers.classifiers
classifiers (Locale ZH _) = ZH_XXClassifiers.classifiers
