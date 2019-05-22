-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Numeral.IS.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral

zeroToTwentyMap:: HashMap Text Integer
zeroToTwentyMap = HashMap.fromList
  [ ( "núll", 0 )
  , ( "null", 0 )
  , ( "einn", 1 )
  , ( "tveir", 2 )
  , ( "þrír", 3 )
  , ( "fjórir", 4 )
  , ( "fimm", 5 )
  , ( "sex", 6 )
  , ( "sjö", 7 )
  , ( "átta", 8 )
  , ( "níu", 9 )
  , ( "tíu", 10 )
  , ( "ellefu", 11 )
  , ( "tólf", 12 )
  , ( "þrettán", 13 )
  , ( "fjórtán", 14 )
  , ( "fimmtán", 15 )
  , ( "sextán", 16 )
  , ( "sautján", 17 )
  , ( "átján", 18 )
  , ( "nítján", 19 )
  , ( "tuttugu", 20 )
  ]

ruleZeroToTwenty :: Rule
ruleZeroToTwenty = Rule
  { name = "number (0..20)"
  , pattern =
    [ regex "(n[úu]ll|einn|tveir|þrír|fjórir|fimm(tán)?|sex(tán)?|sjö|átta|níu|tíu|ellefu|tólf|þrettán|fjórtán|sautján|átján|nítján|tuttugu)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) zeroToTwentyMap >>= integer
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleZeroToTwenty
  ]
