-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Numeral.AR.EG.Rules (rules) where

import Data.Maybe
import Prelude
import qualified Data.HashMap.Strict as HashMap

import Duckling.Dimensions.Types
import Duckling.Numeral.AR.EG.Helpers
  ( digitsMap
  , parseArabicDoubleFromText
  , parseArabicIntegerFromText
  )
import Duckling.Numeral.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral

ruleInteger2 :: Rule
ruleInteger2 = Rule
  { name = "integer 2"
  , pattern =
    [ regex "[إأا]?تني[ي]?ن"
    ]
  , prod = \_ -> integer 2
  }

ruleInteger3 :: Rule
ruleInteger3 = Rule
  { name = "integer 3"
  , pattern =
    [ regex "تلات[هة]?"
    ]
  , prod = \_ -> integer 3
  }

ruleInteger8 :: Rule
ruleInteger8 = Rule
  { name = "integer 8"
  , pattern =
    [ regex "تمان(ي[هة])?"
    ]
  , prod = \_ -> integer 8
  }

ruleInteger11 :: Rule
ruleInteger11 = Rule
  { name = "integer 11"
  , pattern =
    [ regex "(إأا)?حداشر"
    ]
  , prod = \_ -> integer 11
  }

ruleInteger12 :: Rule
ruleInteger12 = Rule
  { name = "integer 12"
  , pattern =
    [ regex "[إأا]?[تط]ناشر"
    ]
  , prod = \_ -> integer 12
  }

ruleInteger13 :: Rule
ruleInteger13 = Rule
  { name = "integer 13"
  , pattern =
    [ regex "[تط]ل(ا)?[تط]اشر"
    ]
  , prod = \_ -> integer 13
  }

ruleInteger14 :: Rule
ruleInteger14 = Rule
  { name = "integer 14"
  , pattern =
    [ regex "[إأا]ربع[تط]اشر"
    ]
  , prod = \_ -> integer 14
  }

ruleInteger15 :: Rule
ruleInteger15 = Rule
  { name = "integer 15"
  , pattern =
    [ regex "خمس[تط]اشر"
    ]
  , prod = \_ -> integer 15
  }

ruleInteger16 :: Rule
ruleInteger16 = Rule
  { name = "integer 16"
  , pattern =
    [ regex "س[تط]اشر"
    ]
  , prod = \_ -> integer 16
  }

ruleInteger17 :: Rule
ruleInteger17 = Rule
  { name = "integer 17"
  , pattern =
    [ regex "سبع[تط]اشر"
    ]
  , prod = \_ -> integer 17
  }

ruleInteger18 :: Rule
ruleInteger18 = Rule
  { name = "integer 18"
  , pattern =
    [ regex "[تط]من[تط]اشر"
    ]
  , prod = \_ -> integer 18
  }

ruleInteger19 :: Rule
ruleInteger19 = Rule
  { name = "integer 19"
  , pattern =
    [ regex "تسع[تط]اشر"
    ]
  , prod = \_ -> integer 19
  }

ruleInteger30_80 :: Rule
ruleInteger30_80 = Rule
  { name = "integer (30, 80)"
  , pattern =
    [ regex "([تط]لا[تط]|[تط]مان)(ين)"
    ]
  , prod = \tokens -> case tokens of
      Token RegexMatch (GroupMatch (match:_)):_ ->
        (* 10) <$> HashMap.lookup match digitsMap >>= integer
      _ -> Nothing
  }

ruleInteger100 :: Rule
ruleInteger100 = Rule
  { name = "integer (100)"
  , pattern =
    [ regex "مي[هةت]"
    ]
  , prod = const $ integer 100
  }

ruleInteger200 :: Rule
ruleInteger200 = Rule
  { name = "integer (200)"
  , pattern =
    [ regex "م(ي)?تين"
    ]
  , prod = const $ integer 200
  }

ruleInteger300 :: Rule
ruleInteger300 = Rule
  { name = "integer (300)"
  , pattern =
    [ regex "([تط]و?لا?[تط]و?)مي[هةت]"
    ]
  , prod = const $ integer 300
  }

ruleInteger400 :: Rule
ruleInteger400 = Rule
  { name = "integer (400)"
  , pattern =
    [ regex "(رو?بعو?)مي[هةت]"
    ]
  , prod = const $ integer 400
  }

ruleInteger500 :: Rule
ruleInteger500 = Rule
  { name = "integer (500)"
  , pattern =
    [ regex "(خو?مسو?)مي[هةت]"
    ]
  , prod = const $ integer 500
  }

ruleInteger600 :: Rule
ruleInteger600 = Rule
  { name = "integer (600)"
  , pattern =
    [ regex "(سو?تو?)مي[هةت]"
    ]
  , prod = const $ integer 600
  }

ruleInteger700 :: Rule
ruleInteger700 = Rule
  { name = "integer (700)"
  , pattern =
    [ regex "(سو?بعو?)مي[هةت]"
    ]
  , prod = const $ integer 700
  }

ruleInteger800 :: Rule
ruleInteger800 = Rule
  { name = "integer (800)"
  , pattern =
    [ regex "([تط]و?منو?)مي[هةت]"
    ]
  , prod = const $ integer 800
  }

ruleInteger900 :: Rule
ruleInteger900 = Rule
  { name = "integer (900)"
  , pattern =
    [ regex "([تط]و?سعو?)مي[هةت]"
    ]
  , prod = const $ integer 900
  }

ruleInteger101_999 :: Rule
ruleInteger101_999 = Rule
  { name = "integer 101..999"
  , pattern =
    [ oneOf [100, 200 .. 900]
    , regex "\\s"
    , oneOf $ map (+) [10, 20 .. 90] <*> [1..9]
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v1}:
       _:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v1 + v2
      _ -> Nothing
  }


ruleInteger3000 :: Rule
ruleInteger3000 = Rule
  { name = "integer (3000)"
  , pattern =
    [ regex "([تط]لا?[تط])( ?[اآ])?لاف"
    ]
  , prod = const $ integer 3000
  }

ruleInteger4000 :: Rule
ruleInteger4000 = Rule
  { name = "integer (4000)"
  , pattern =
    [ regex "[أا]ربع(ت| ?[اآ])لاف"
    ]
  , prod = const $ integer 4000
  }

ruleInteger5000 :: Rule
ruleInteger5000 = Rule
  { name = "integer (5000)"
  , pattern =
    [ regex "خمس(ت| ?[اآ])لاف"
    ]
  , prod = const $ integer 5000
  }

ruleInteger6000 :: Rule
ruleInteger6000 = Rule
  { name = "integer (6000)"
  , pattern =
    [ regex "س(ت| ?[اآ])لاف"
    ]
  , prod = const $ integer 6000
  }

ruleInteger7000 :: Rule
ruleInteger7000 = Rule
  { name = "integer (7000)"
  , pattern =
    [ regex "سبع(ت| ?[اآ])لاف"
    ]
  , prod = const $ integer 7000
  }

ruleInteger8000 :: Rule
ruleInteger8000 = Rule
  { name = "integer (8000)"
  , pattern =
    [ regex "[تط]م[ا]?ن(ت| ?[اآ])لاف"
    ]
  , prod = const $ integer 8000
  }

ruleInteger9000 :: Rule
ruleInteger9000 = Rule
  { name = "integer (9000)"
  , pattern =
    [ regex "([تط]سع)(ت| ?[اآ])لاف"
    ]
  , prod = const $ integer 9000
  }

rules :: [Rule]
rules =
  [ ruleInteger2
  , ruleInteger3
  , ruleInteger8
  , ruleInteger11
  , ruleInteger12
  , ruleInteger13
  , ruleInteger14
  , ruleInteger15
  , ruleInteger16
  , ruleInteger17
  , ruleInteger18
  , ruleInteger19
  , ruleInteger30_80
  , ruleInteger100
  , ruleInteger200
  , ruleInteger300
  , ruleInteger400
  , ruleInteger500
  , ruleInteger600
  , ruleInteger700
  , ruleInteger800
  , ruleInteger900
  , ruleInteger101_999
  , ruleInteger3000
  , ruleInteger4000
  , ruleInteger5000
  , ruleInteger6000
  , ruleInteger7000
  , ruleInteger8000
  , ruleInteger9000
  ]
