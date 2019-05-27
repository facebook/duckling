-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE LambdaCase #-}

module Duckling.Numeral.ML.Rules
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


zeroToNineMap :: HashMap Text Integer
zeroToNineMap = HashMap.fromList
  [ ( "പൂജ്യം", 0 )
  , ( "ഒന്ന്", 1 )
  , ( "രണ്ട്", 2 )
  , ( "മുന്ന്", 3 )
  , ( "നാല്", 4 )
  , ( "അഞ്ച്", 5 )
  , ( "ആറ്", 6 )
  , ( "ഏഴ്", 7 )
  , ( "എട്ട്", 8 )
  , ( "ഒൻപത്", 9 )
  ]

ruleZeroToNine :: Rule
ruleZeroToNine = Rule
  { name = "integer (0..9)"
  , pattern =
    [ regex "(പൂജ്യം|ഒന്ന്|രണ്ട്|മുന്ന്|നാല്|അഞ്ച്|ആറ്|ഏഴ്|എട്ട്|ഒൻപത്)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) zeroToNineMap >>= integer
      _ -> Nothing
  }

tenToNineteenMap :: HashMap Text Integer
tenToNineteenMap = HashMap.fromList
  [ ( "പത്ത്", 10 )
  , ( "പതിനൊന്ന്", 11 )
  , ( "പന്ത്രണ്ട്", 12 )
  , ( "പതിമൂന്ന്", 13 )
  , ( "പതിനാല്", 14 )
  , ( "പതിനഞ്ച്", 15 )
  , ( "പതിനാറ്", 16 )
  , ( "പതിനേഴ്", 17 )
  , ( "പതിനെട്ട്", 18 )
  , ( "പത്തൊമ്പത്", 19 )
  ]

ruleTenToNineteen :: Rule
ruleTenToNineteen = Rule
  { name = "integer (10..19)"
  , pattern =
    [
      regex "(പത്ത്|പതിനൊന്ന്|പന്ത്രണ്ട്|പതിമൂന്ന്|പതിനാല്|പതിനഞ്ച്|പതിനാറ്|പതിനേഴ്|പതിനെട്ട്|പത്തൊമ്പത്)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) tenToNineteenMap >>= integer
      _ -> Nothing
  }

tensMap :: HashMap Text Integer
tensMap = HashMap.fromList
  [ ( "ഇരുപത്", 20 )
  , ( "ഇരുപത്തി", 20 )
  , ( "മുപ്പത്", 30 )
  , ( "മുപ്പത്തി", 30 )
  , ( "നാല്പത്", 40 )
  , ( "നാല്പത്തി", 40 )
  , ( "അമ്പത്", 50 )
  , ( "അമ്പത്തി", 50 )
  , ( "അറുപത്", 60 )
  , ( "അറുപത്തി", 60 )
  , ( "എഴുപത്", 70 )
  , ( "എഴുപത്തി", 70 )
  , ( "എൺപത്", 80 )
  , ( "എൺപത്തി", 80 )
  , ( "തൊണ്ണൂറ്", 90 )
  , ( "തൊണ്ണൂറ്റി", 90 )
  ]

ruleTens :: Rule
ruleTens = Rule
  { name = "integer (20..90)"
  , pattern =
    [ regex "(ഇരുപത്|മുപ്പത്|നാല്പത്|അമ്പത്|അറുപത്|എഴുപത്|എൺപത്|തൊണ്ണൂറ്)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) tensMap >>= integer
      _ -> Nothing
  }

ruleCompositeTens :: Rule
ruleCompositeTens = Rule
  { name = "integer ([2-9][1-9])"
  , pattern =
    [ regex "(ഇരുപത്തി|മുപ്പത്തി|നാല്പത്തി|അമ്പത്തി|അറുപത്തി|എഴുപത്തി|എൺപത്തി|തൊണ്ണൂറ്റി)(ഒന്ന്|രണ്ട്|മുന്ന്|നാല്|അഞ്ച്|ആറ്|ഏഴ്|എട്ട്|ഒൻപത്)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        v1 <- HashMap.lookup (Text.toLower m1) tensMap
        v2 <- HashMap.lookup (Text.toLower m2) zeroToNineMap
        integer $ v1 + v2
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleZeroToNine
  , ruleTenToNineteen
  , ruleCompositeTens
  , ruleTens
  ]
