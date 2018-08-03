-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Numeral.KM.Rules
  ( rules ) where

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
  [ ( "០", 0 )
  , ( "១", 1 )
  , ( "២", 2 )
  , ( "៣", 3 )
  , ( "៤", 4 )
  , ( "៥", 5 )
  , ( "៦", 6 )
  , ( "៧", 7 )
  , ( "៨", 8 )
  , ( "៩", 9 )
  ]

ruleNumeral :: Rule
ruleNumeral = Rule
  { name = "number (0..9)"
  , pattern =
    [ regex "(០|១|២|៣|៤|៥|៦|៧|៨|៩)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup match ruleNumeralMap >>= integer
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleNumeral
  ]
