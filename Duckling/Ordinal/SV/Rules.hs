-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.SV.Rules
  ( rules ) where

import Control.Monad (join)
import Data.HashMap.Strict ( HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Helpers
import Duckling.Regex.Types
import Duckling.Types

ordinalsMap :: HashMap Text Int
ordinalsMap = HashMap.fromList
  [ ( "f\x00f6rsta", 1 )
  , ( "f\x00f6rste", 1 )
  , ( "andra", 2 )
  , ( "andre", 2)
  , ( "tredje", 3 )
  , ( "fj\x00e4rde", 4 )
  , ( "femte", 5 )
  , ( "sj\x00e4tte", 6 )
  , ( "sjunde", 7 )
  , ( "\x00e5ttonde", 8 )
  , ( "nionde", 9 )
  , ( "tionde", 10 )
  , ( "elfte", 11 )
  , ( "tolfte", 12 )
  , ( "trettonde", 13 )
  , ( "fjortonde", 14 )
  , ( "femtonde", 15 )
  , ( "sextonde", 16 )
  , ( "sjuttonde", 17 )
  , ( "artonde", 18 )
  , ( "nittonde", 19 )
  , ( "tjugonde", 20 )
  , ( "trettionde", 30 )
  , ( "fyrtionde", 40 )
  , ( "femtionde", 50 )
  , ( "sextionde", 60 )
  , ( "sjuttionde", 70 )
  , ( "\x00e5ttionde", 80 )
  , ( "nittionde", 90 )
  ]

cardinalsMap :: HashMap Text Int
cardinalsMap = HashMap.fromList
  [ ( "tjugo", 20 )
  , ( "trettio", 30 )
  , ( "fyrtio", 40 )
  , ( "femtio", 50 )
  , ( "sextio", 60 )
  , ( "sjuttio", 70 )
  , ( "\x00e5ttio", 80 )
  , ( "nittio", 90 )
  ]


ruleOrdinals :: Rule
ruleOrdinals = Rule
  { name = "ordinals (first..twentieth,thirtieth,...)"
  , pattern = [regex "(f\x00f6rsta|f\x00f6rste|andra|andre|tredje|fj\x00e4rde|femte|sj\x00e4tte|sjunde|\x00e5ttonde|nionde|tionde|elfte|tolfte|trettionde|fjortonde|femtonde|sextonde|sjuttonde|artonde|nittonde|sjugonde|trettionde|fyrtionde|femtonde|sextionde|sjuttionde|\x00e5ttionde|nittionde)"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> HashMap.lookup (Text.toLower match) ordinalsMap
      _ -> Nothing
    }

ruleCompositeOrdinals :: Rule
ruleCompositeOrdinals = Rule
  { name = "ordinals (composite, e.g., eighty-seven)"
  , pattern = [regex "(tjugo|trettio|fyrtio|femtio|sextio|sjuttio|\x00e5ttio|nittio)(f\x00f6rsta|f\x00f6rste|andra|andre|tredje|fj\x00e4rde|femte|sj\x00e4tte|sjunde|\x00e5ttonde|nionde)"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (tens:units:_)):_) -> do
        tt <- HashMap.lookup (Text.toLower tens) cardinalsMap
        uu <- HashMap.lookup (Text.toLower units) ordinalsMap
        Just (ordinal (tt + uu))
      _ -> Nothing
  }

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern = [regex "0*(\\d+):?(a|e)"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> ordinal <$> parseInt match
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleOrdinals
  , ruleCompositeOrdinals
  , ruleOrdinalDigits
  ]
