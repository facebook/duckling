-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Ordinal.ML.Rules
  ( rules ) where

import Data.HashMap.Strict (HashMap)
import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Helpers
import Duckling.Regex.Types
import Duckling.Types

oneToNineteenMap :: HashMap Text Int
oneToNineteenMap = HashMap.fromList
  [ ( "ഒന്നാം", 1 )
  , ( "രണ്ടാം", 2 )
  , ( "മൂന്നാം", 3 )
  , ( "നാലാം", 4 )
  , ( "അഞ്ചാം", 5 )
  , ( "ആറാം", 6 )
  , ( "ഏഴാം", 7 )
  , ( "എട്ടാം", 8 )
  , ( "ഒമ്പതാം", 9 )
  , ( "പത്താം", 10 )
  , ( "പതിനൊന്നാം", 11 )
  , ( "പന്ത്രണ്ടാം", 12 )
  , ( "പതിമൂന്നാം", 13 )
  , ( "പതിനാലാം", 14 )
  , ( "പതിനഞ്ചാം", 15 )
  , ( "പതിനാറാം", 16 )
  , ( "പതിനേഴാം", 17 )
  , ( "പതിനെട്ടാം", 18 )
  , ( "പത്തൊൻപതാം", 19 )
  ]

ruleOneToNineteen :: Rule
ruleOneToNineteen = Rule
  { name = "integer (1..19)"
  , pattern =
    [ regex "(ഒന്നാം|രണ്ടാം|മൂന്നാം|നാലാം|അഞ്ചാം|ആറാം|ഏഴാം|എട്ടാം|ഒമ്പതാം|പത്താം|പതിനൊന്നാം|പന്ത്രണ്ടാം|പതിമൂന്നാം|പതിനാലാം|പതിനഞ്ചാം|പതിനാറാം|പതിനേഴാം|പതിനെട്ടാം|പത്തൊൻപതാം)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> HashMap.lookup (Text.toLower match) oneToNineteenMap
      _ -> Nothing
  }

tensMap :: HashMap Text Int
tensMap = HashMap.fromList
  [ ( "ഇരുപതാം", 20 )
  , ( "മുപ്പത്തഞ്ചാം", 30 )
  , ( "നാല്പതാം", 40 )
  , ( "അമ്പതാം", 50 )
  , ( "അറുപതാം", 60 )
  , ( "എഴുപത്താം", 70 )
  , ( "എൺപത്താം", 80 )
  , ( "തൊണ്ണൂറാം", 90 )
  ]

ruleTens :: Rule
ruleTens = Rule
  { name = "integer (20..90)"
  , pattern =
    [ regex "(ഇരുപതാം|മുപ്പത്തഞ്ചാം|നാല്പതാം|അമ്പതാം|അറുപതാം|എഴുപത്താം|എൺപത്താം|തൊണ്ണൂറാം)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> HashMap.lookup (Text.toLower match) tensMap
      _ -> Nothing
  }

tensOrdinalMap :: HashMap Text Int
tensOrdinalMap = HashMap.fromList
  [ ( "ഇരുപത്തി", 20 )
  , ( "മുപ്പത്തി", 30 )
  , ( "നാല്പത്തി", 40 )
  , ( "അമ്പത്തി", 50 )
  , ( "അറുപത്തി", 60 )
  , ( "എഴുപത്തി", 70 )
  , ( "എൺപത്തി", 80 )
  , ( "തൊണ്ണൂറ്റി", 90 )
  ]

oneToNineMap :: HashMap Text Int
oneToNineMap = HashMap.filterWithKey (\_ v -> v <= 9) oneToNineteenMap


ruleCompositeTens :: Rule
ruleCompositeTens = Rule
  { name = "integer ([2-9][1-9])"
  , pattern =
    [ regex "(ഇരുപത്തി|മുപ്പത്തി|നാല്പത്തി|അമ്പത്തി|അറുപത്തി|എഴുപത്തി|എൺപത്തി|തൊണ്ണൂറ്റി)(ആദ്യം|രണ്ടാം|മൂന്നാം|നാലാം|അഞ്ചാം|ആറാം|ഏഴാം|എട്ടാം|ഒമ്പത)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        v1 <- HashMap.lookup (Text.toLower m1) tensOrdinalMap
        v2 <- HashMap.lookup (Text.toLower m2) oneToNineMap
        Just $ ordinal (v1 + v2)
      _ -> Nothing
  }

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern =
    [ regex "0*(\\d+)\\."
    ]
  , prod = \case
    (   Token RegexMatch (GroupMatch (match :_)) : _)
          -> ordinal <$> parseInt match
    _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleOrdinalDigits
  , ruleOneToNineteen
  , ruleTens
  , ruleCompositeTens
  ]
