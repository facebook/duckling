-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.HR.Rules
  ( rules ) where

import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Helpers
import Duckling.Ordinal.Types (OrdinalData (..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Ordinal.Types as TOrdinal

ordinalsMap :: HashMap Text Int
ordinalsMap = HashMap.fromList
  [ ("prva", 1)
  , ("prvi", 1)
  , ("prvoga", 1)
  , ("prvo", 1)
  , ("prvog", 1)
  , ("drugoga", 2)
  , ("drugo", 2)
  , ("druga", 2)
  , ("drugi", 2)
  , ("drugog", 2)
  , ("tre\263i", 3)
  , ("tre\263ega", 3)
  , ("trece", 3)
  , ("tre\263eg", 3)
  , ("treca", 3)
  , ("tre\263a", 3)
  , ("treci", 3)
  , ("\269etvrti", 4)
  , ("\269etvrtoga", 4)
  , ("cetvrtoga", 4)
  , ("cetvrto", 4)
  , ("cetvrtog", 4)
  , ("cetvrti", 4)
  , ("\269etvrtog", 4)
  , ("cetvrta", 4)
  , ("\269etvrto", 4)
  , ("\269etvrta", 4)
  , ("peto", 5)
  , ("peti", 5)
  , ("petoga", 5)
  , ("petog", 5)
  , ("peta", 5)
  , ("sesti", 6)
  , ("\353estoga", 6)
  , ("\353estog", 6)
  , ("sestoga", 6)
  , ("sesta", 6)
  , ("\353esta", 6)
  , ("\353esto", 6)
  , ("\353esti", 6)
  , ("sesto", 6)
  , ("sestog", 6)
  , ("sedmi", 7)
  , ("sedmog", 7)
  , ("sedma", 7)
  , ("sedmo", 7)
  , ("sedmoga", 7)
  , ("osmo", 8)
  , ("osmog", 8)
  , ("osma", 8)
  , ("osmi", 8)
  , ("osmoga", 8)
  , ("devetog", 9)
  , ("deveta", 9)
  , ("deveto", 9)
  , ("devetoga", 9)
  , ("deveti", 9)
  , ("desetoga", 10)
  , ("desetog", 10)
  , ("deseta", 10)
  , ("deseto", 10)
  , ("deseti", 10)
  , ("jedanaestoga", 11)
  , ("jedanaesta", 11)
  , ("jedanaesti", 11)
  , ("jedanaesto", 11)
  , ("jedanaestog", 11)
  , ("dvanaesto", 12)
  , ("dvanaestog", 12)
  , ("dvanaesti", 12)
  , ("dvanaesta", 12)
  , ("dvanaestoga", 12)
  , ("trinaesta", 13)
  , ("trinaestoga", 13)
  , ("trinaestog", 13)
  , ("trinaesto", 13)
  , ("trinaesti", 13)
  , ("cetrnaestog", 14)
  , ("cetrnaesto", 14)
  , ("cetrnaesti", 14)
  , ("\269etrnaestoga", 14)
  , ("cetrnaesta", 14)
  , ("\269etrnaesto", 14)
  , ("\269etrnaesta", 14)
  , ("cetrnaestoga", 14)
  , ("\269etrnaesti", 14)
  , ("\269etrnaestog", 14)
  , ("petnaestog", 15)
  , ("petnaesto", 15)
  , ("petnaesta", 15)
  , ("petnaesti", 15)
  , ("petnaestoga", 15)
  , ("sesnaesto", 16)
  , ("sesnaestog", 16)
  , ("sesnaesti", 16)
  , ("\353esnaestoga", 16)
  , ("sesnaesta", 16)
  , ("\353esnaesto", 16)
  , ("sesnaestoga", 16)
  , ("\353esnaesti", 16)
  , ("\353esnaesta", 16)
  , ("\353esnaestog", 16)
  , ("sedamnaesto", 17)
  , ("sedamnaestoga", 17)
  , ("sedamnaesti", 17)
  , ("sedamnaesta", 17)
  , ("sedamnaestog", 17)
  , ("osamnaestoga", 18)
  , ("osamnaestog", 18)
  , ("osamnaesti", 18)
  , ("osamnaesta", 18)
  , ("osamnaesto", 18)
  , ("devetnaestoga", 19)
  , ("devetnaestog", 19)
  , ("devetnaesto", 19)
  , ("devetnaesti", 19)
  , ("devetnaesta", 19)
  ]

ruleOrdinalsFirstth :: Rule
ruleOrdinalsFirstth = Rule
  { name = "ordinals (first..19th)"
  , pattern =
    [ regex "(prv(i|a|o(ga?)?)|drug(i|a|o(ga?)?)|tre(c|ć)(i|a|e(ga?)?)|(č|c)etvrt(i|a|o(ga?)?)|pet(i|a|o(ga?)?)|(š|s)est(i|a|o(ga?)?)|sedm(i|a|o(ga?)?)|osm(i|a|o(ga?)?)|devet(i|a|o(ga?)?)|deset(i|a|o(ga?)?)|jedanaest(i|a|o(ga?)?)|dvanaest(i|a|o(ga?)?)|trinaest(i|a|o(ga?)?)|(c|č)etrnaest(i|a|o(ga?)?)|petnaest(i|a|o(ga?)?)|(s|š)esnaest(i|a|o(ga?)?)|sedamnaest(i|a|o(ga?)?)|osamnaest(i|a|o(ga?)?)|devetnaest(i|a|o(ga?)?))"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> HashMap.lookup (Text.toLower match) ordinalsMap
      _ -> Nothing
  }

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern =
    [ regex "0*(\\d+)(\\.| ?(t(i|a)(n|r|s)?)|(ste(n|r|s)?))"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> ordinal <$> parseInt match
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleOrdinalDigits
  , ruleOrdinalsFirstth
  ]
