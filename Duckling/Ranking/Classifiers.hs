-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


module Duckling.Ranking.Classifiers
  ( classifiers
  ) where

import Duckling.Lang
import qualified Duckling.Ranking.Classifiers.AR as ARClassifiers
import qualified Duckling.Ranking.Classifiers.BG as BGClassifiers
import qualified Duckling.Ranking.Classifiers.CS as CSClassifiers
import qualified Duckling.Ranking.Classifiers.DA as DAClassifiers
import qualified Duckling.Ranking.Classifiers.DE as DEClassifiers
import qualified Duckling.Ranking.Classifiers.EN as ENClassifiers
import qualified Duckling.Ranking.Classifiers.ES as ESClassifiers
import qualified Duckling.Ranking.Classifiers.ET as ETClassifiers
import qualified Duckling.Ranking.Classifiers.FR as FRClassifiers
import qualified Duckling.Ranking.Classifiers.GA as GAClassifiers
import qualified Duckling.Ranking.Classifiers.HE as HEClassifiers
import qualified Duckling.Ranking.Classifiers.HR as HRClassifiers
import qualified Duckling.Ranking.Classifiers.HU as HUClassifiers
import qualified Duckling.Ranking.Classifiers.ID as IDClassifiers
import qualified Duckling.Ranking.Classifiers.IT as ITClassifiers
import qualified Duckling.Ranking.Classifiers.JA as JAClassifiers
import qualified Duckling.Ranking.Classifiers.KO as KOClassifiers
import qualified Duckling.Ranking.Classifiers.MY as MYClassifiers
import qualified Duckling.Ranking.Classifiers.NB as NBClassifiers
import qualified Duckling.Ranking.Classifiers.NL as NLClassifiers
import qualified Duckling.Ranking.Classifiers.PL as PLClassifiers
import qualified Duckling.Ranking.Classifiers.PT as PTClassifiers
import qualified Duckling.Ranking.Classifiers.RO as ROClassifiers
import qualified Duckling.Ranking.Classifiers.RU as RUClassifiers
import qualified Duckling.Ranking.Classifiers.SV as SVClassifiers
import qualified Duckling.Ranking.Classifiers.TR as TRClassifiers
import qualified Duckling.Ranking.Classifiers.UK as UKClassifiers
import qualified Duckling.Ranking.Classifiers.VI as VIClassifiers
import qualified Duckling.Ranking.Classifiers.ZH as ZHClassifiers
import Duckling.Ranking.Types

classifiers :: Lang -> Classifiers
classifiers AR = ARClassifiers.classifiers
classifiers BG = BGClassifiers.classifiers
classifiers CS = CSClassifiers.classifiers
classifiers DA = DAClassifiers.classifiers
classifiers DE = DEClassifiers.classifiers
classifiers EN = ENClassifiers.classifiers
classifiers ES = ESClassifiers.classifiers
classifiers ET = ETClassifiers.classifiers
classifiers FR = FRClassifiers.classifiers
classifiers GA = GAClassifiers.classifiers
classifiers HE = HEClassifiers.classifiers
classifiers HR = HRClassifiers.classifiers
classifiers HU = HUClassifiers.classifiers
classifiers ID = IDClassifiers.classifiers
classifiers IT = ITClassifiers.classifiers
classifiers JA = JAClassifiers.classifiers
classifiers KO = KOClassifiers.classifiers
classifiers MY = MYClassifiers.classifiers
classifiers NB = NBClassifiers.classifiers
classifiers NL = NLClassifiers.classifiers
classifiers PL = PLClassifiers.classifiers
classifiers PT = PTClassifiers.classifiers
classifiers RO = ROClassifiers.classifiers
classifiers RU = RUClassifiers.classifiers
classifiers SV = SVClassifiers.classifiers
classifiers TR = TRClassifiers.classifiers
classifiers UK = UKClassifiers.classifiers
classifiers VI = VIClassifiers.classifiers
classifiers ZH = ZHClassifiers.classifiers
