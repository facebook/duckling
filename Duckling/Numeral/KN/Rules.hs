-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Numeral.KN.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Regex.Types
import Duckling.Types

ankiMap :: HashMap Char Char
ankiMap = HashMap.fromList
  [ ('೦', '0')
  , ('೧', '1')
  , ('೨', '2')
  , ('೩', '3')
  , ('೪', '4')
  , ('೫', '5')
  , ('೬', '6')
  , ('೭', '7')
  , ('೮', '8')
  , ('೯', '9')
  ]

ankiToArab :: Char -> Char
ankiToArab c = HashMap.lookupDefault c c ankiMap

ruleAnki :: Rule
ruleAnki = Rule
  { name = "anki forms"
  , pattern =
    [ regex "([೦೧೨೩೪೫೬೭೮೯]{1,10})"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        toInteger <$> parseInt (Text.map ankiToArab match) >>= integer
      _ -> Nothing
  }

ruleNumeralMap :: [(Text, Integer)]
ruleNumeralMap =
  [ ("ಸೊನ್ನೆ", 0)
  , ("ಒಂದು", 1)
  , ("ಎರಡು", 2)
  , ("ಮೂರು", 3)
  , ("ನಾಲ್ಕು", 4)
  , ("ಐದು", 5)
  , ("ಆರು", 6)
  , ("ಏಳು", 7)
  , ("ಎಂಟು", 8)
  , ("ಒಂಬತ್ತು", 9)
  ]

ruleNumerals :: [Rule]
ruleNumerals =
  map constructRule ruleNumeralMap
  where
    constructRule :: (Text, Integer) -> Rule
    constructRule (s, i) = Rule
      { name = "number: " `mappend` s
      , pattern =
        [ regex $ Text.unpack s
        ]
      , prod = const $ integer i
      }


rules :: [Rule]
rules =
  [ ruleAnki
  ]
  ++ ruleNumerals
