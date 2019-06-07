-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.CS.Rules
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

ruleNumeralMap :: HashMap Text Integer
ruleNumeralMap = HashMap.fromList
  [ ( "nula", 0 )
  , ( "jeden", 1 )
  , ( "jedna", 1 )
  , ( "jedno", 1 )
  , ( "dva", 2 )
  , ( "dv\x0115", 2 )
  , ( "t\x0159i", 3 )
  , ( "čty\x0159i", 4 )
  , ( "p\x0115t", 5)
  , ( "šest", 6)
  , ( "sedm", 7)
  , ( "osm", 8)
  , ( "dev\x0115t", 9)
  , ( "deset", 10)
  ]

ruleNumeral :: Rule
ruleNumeral = Rule
  { name = "number (0..10)"
  , pattern =
    [ regex "(nula|jed(en|n[ao])|dv(a|\x0115)|t(\x0159)i|(č)ty(\x0159)i|p(\x0115)t|(š)est|sedm|osm|dev(\x0115)t|deset)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) ruleNumeralMap >>= integer
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleNumeral
  ]
