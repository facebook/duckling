-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Numeral.KN.Rules
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

ankiMap :: HashMap Char Char
ankiMap = HashMap.fromList
	[ ( '೦', '0' )
	, ( '೧', '1' )
	, ( '೨', '2' )
	, ( '೩', '3' )
	, ( '೪', '4' )
	, ( '೫', '5' )
	, ( '೬', '6' )
	, ( '೭', '7' )
	, ( '೮', '8' )
	, ( '೯', '9' )
	]

ankiToArab :: Char -> Char
ankiToArab c = HashMap.lookupDefault c c ankiMap

ruleAnki :: Rule
ruleAnki = Rule
	{	name = "anki forms"
	, pattern = 
		[	regex "([೦೧೨೩೪೫೬೭೮೯]{1,10})"
		]
	, prod = \tokens -> case tokens of
			(Token RegexMatch (GroupMatch (match:_)):_) ->
				toInteger <$> parseInt (Text.map ankiToArab match) >>= integer
			_ -> Nothing
	}

ruleNumeralMap :: HashMap Text Integer
ruleNumeralMap = HashMap.fromList
	[ ( "ಸೊನ್ನೆ", 0 )
	, ( "ಒಂದು", 1 )
	, ( "ಎರಡು", 2 )
    , ( "ಮೂರು", 3 )
    , ( "ನಾಲ್ಕು", 4 )
    , ( "ಅಯ್ದು", 5 )
    , ( "ಆರು", 6 )
    , ( "ಏಳು", 7 )
    , ( "ಎಂಟು", 8 )
    , ( "ಒಂಬತ್ತು", 9 )
	]

ruleNumeral :: Rule
ruleNumeral = Rule
  { name = "number (0..10)"
  , pattern =
    [ regex "(ಸೊನ್ನೆ|ಒಂದು|ಎರಡು|ಮೂರು|ನಾಲ್ಕು|ಅಯ್ದು|ಆರು|ಏಳು|ಎಂಟು|ಒಂಬತ್ತು)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup match ruleNumeralMap >>= integer
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleAnki
  , ruleNumeral
  ]