-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.UK.Rules
  ( rules ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Control.Monad (join)
import Data.Text (Text)
import qualified Data.Text as Text
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Helpers
import Duckling.Regex.Types
import Duckling.Types

ordinalsFirstThMap :: HashMap Text Int
ordinalsFirstThMap = HashMap.fromList
  [ ( "\x043f\x0435\x0440\x0448"                   , 1 )
  , ( "\x0434\x0440\x0443\x0433"                   , 2 )
  , ( "\x0442\x0440\x0435\x0442"                   , 3 )
  , ( "\x0447\x0435\x0442\x0432\x0435\x0440\x0442" , 4 )
  , ( "\x043f\x2018\x044f\x0442"                   , 5 )
  , ( "\x0448\x043e\x0441\x0442"                   , 6 )
  , ( "\x0441\x044c\x043e\x043c"                   , 7 )
  , ( "\x0432\x043e\x0441\x044c\x043c"             , 8 )
  , ( "\x0434\x0435\x0432\x2018\x044f\x0442"       , 9 )
  , ( "\x0434\x0435\x0441\x044f\x0442"             , 10 )
  , ( "\x043e\x0434\x0438\x043d\x0430\x0434\x0446\x044f\x0442"             , 11 )
  , ( "\x0434\x0432\x0430\x043d\x0430\x0434\x0446\x044f\x0442"             , 12 )
  , ( "\x0442\x0440\x0438\x043d\x0430\x0434\x0446\x044f\x0442"             , 13 )
  , ( "\x0447\x043e\x0442\x0438\x0440\x043d\x0430\x0434\x0446\x044f\x0442" , 14 )
  , ( "\x043f\x2018\x044f\x0442\x043d\x0430\x0434\x0446\x044f\x0442"       , 15 )
  , ( "\x0448\x0456\x0441\x0442\x043d\x0430\x0434\x0446\x044f\x0442"       , 16 )
  , ( "\x0441\x0456\x043c\x043d\x0430\x0434\x0446\x044f\x0442"             , 17 )
  , ( "\x0432\x0456\x0441\x0456\x043c\x043d\x0430\x0434\x0446\x044f\x0442" , 18 )
  , ( "\x0434\x0435\x0432\x2018\x044f\x0442\x043d\x0430\x0434\x0446\x044f\x0442" , 19 )
  , ( "\x0434\x0432\x0430\x0434\x0446\x044f\x0442"                               , 20 )
  ]

ruleOrdinalsFirstth :: Rule
ruleOrdinalsFirstth = Rule
  { name = "ordinals (first..19th)"
  , pattern =
    [ regex "(\x043f\x0435\x0440\x0448|\x0434\x0440\x0443\x0433|\x0442\x0440\x0435\x0442|\x0447\x0435\x0442\x0432\x0435\x0440\x0442|\x043f\x2018\x044f\x0442|\x0448\x043e\x0441\x0442|\x0441\x044c\x043e\x043c|\x0432\x043e\x0441\x044c\x043c|\x0434\x0435\x0432\x2018\x044f\x0442|\x0434\x0435\x0441\x044f\x0442|\x043e\x0434\x0438\x043d\x0430\x0434\x0446\x044f\x0442|\x0434\x0432\x0430\x043d\x0430\x0434\x0446\x044f\x0442|\x0442\x0440\x0438\x043d\x0430\x0434\x0446\x044f\x0442|\x0447\x043e\x0442\x0438\x0440\x043d\x0430\x0434\x0446\x044f\x0442|\x043f\x2018\x044f\x0442\x043d\x0430\x0434\x0446\x044f\x0442|\x0448\x0456\x0441\x0442\x043d\x0430\x0434\x0446\x044f\x0442|\x0441\x0456\x043c\x043d\x0430\x0434\x0446\x044f\x0442|\x0432\x0456\x0441\x0456\x043c\x043d\x0430\x0434\x0446\x044f\x0442|\x0434\x0435\x0432\x2018\x044f\x0442\x043d\x0430\x0434\x0446\x044f\x0442|\x0434\x0432\x0430\x0434\x0446\x044f\x0442)(\x0438\x0439|\x0456\x0439|\x0430|\x044f|\x0435|\x0454)"
    ]
  , prod = \tokens -> case tokens of
    (Token RegexMatch (GroupMatch (match:_)):_) ->
      ordinal <$> HashMap.lookup (Text.toLower match) ordinalsFirstThMap
    _ -> Nothing
  }

ordinalTensMap :: HashMap Text Int
ordinalTensMap = HashMap.fromList
  [ ( "\x0434\x0432\x0430\x0434\x0446\x044f\x0442\x044c"             , 20 )
  , ( "\x0442\x0440\x0438\x0434\x0446\x044f\x0442\x044c"             , 30 )
  , ( "\x0441\x043e\x0440\x043e\x043a"                               , 40 )
  , ( "\x043f\x2018\x044f\x0442\x0434\x0435\x0441\x044f\x0442"       , 50 )
  , ( "\x0448\x0456\x0441\x0442\x0434\x0435\x0441\x044f\x0442"       , 60 )
  , ( "\x0441\x0456\x043c\x0434\x0435\x0441\x044f\x0442"             , 70 )
  , ( "\x0432\x0456\x0441\x0456\x043c\x0434\x0435\x0441\x044f\x0442" , 80 )
  , ( "\x0434\x0435\x0432\x2018\x044f\x043d\x043e\x0441\x0442\x043e" , 90 )
  ]

ruleOrdinal :: Rule
ruleOrdinal = Rule
  { name = "ordinal 21..99"
  , pattern =
    [ regex "(\x0434\x0432\x0430\x0434\x0446\x044f\x0442\x044c|\x0442\x0440\x0438\x0434\x0446\x044f\x0442\x044c|\x0441\x043e\x0440\x043e\x043a|\x043f\x2018\x044f\x0442\x0434\x0435\x0441\x044f\x0442|\x0448\x0456\x0441\x0442\x044c\x0434\x0435\x0441\x044f\x0442|\x0441\x0456\x043c\x0434\x0435\x0441\x044f\x0442|\x0432\x0456\x0441\x0456\x043c\x0434\x0435\x0441\x044f\x0442|\x0434\x0435\x0432\x2018\x044f\x043d\x043e\x0441\x0442\x043e)"
    , regex "(\x043f\x0435\x0440\x0448|\x0434\x0440\x0443\x0433|\x0442\x0440\x0435\x0442|\x0447\x0435\x0442\x0432\x0435\x0440\x0442|\x043f\x2018\x044f\x0442|\x0448\x043e\x0441\x0442|\x0441\x044c\x043e\x043c|\x0432\x043e\x0441\x044c\x043c|\x0434\x0435\x0432\x2018\x044f\x0442)(\x0438\x0439|\x0456\x0439|\x0430|\x044f|\x0435|\x0454)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:_)):
       Token RegexMatch (GroupMatch (m2:_)):
       _) -> do
        v1 <- HashMap.lookup (Text.toLower m1) ordinalTensMap
        v2 <- HashMap.lookup (Text.toLower m2) ordinalsFirstThMap -- map to 1..9
        Just . ordinal $ v1 + v2
      _ -> Nothing
  }

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern =
    [ regex "0*(\\d+)-?((\x0438|\x0456)?\x0439|\x0430|\x044f|\x0435|\x0454)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> ordinal <$> parseInt match
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleOrdinal
  , ruleOrdinalDigits
  , ruleOrdinalsFirstth
  ]
