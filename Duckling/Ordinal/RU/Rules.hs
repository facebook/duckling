-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.RU.Rules
  ( rules ) where

import Control.Monad (join)
import qualified Data.Text as Text
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Helpers
import Duckling.Regex.Types
import Duckling.Types

ruleOrdinalsFirstth :: Rule
ruleOrdinalsFirstth = Rule
  { name = "ordinals (first..19th)"
  , pattern =
    [ regex "(\x043f\x0435\x0440\x0432|\x0432\x0442\x043e\x0440|\x0442\x0440\x0435\x0442|\x0447\x0435\x0442\x0432\x0435\x0440\x0442|\x043f\x044f\x0442|\x0448\x0435\x0441\x0442|\x0441\x0435\x0434\x044c\x043c|\x0432\x043e\x0441\x044c\x043c|\x0434\x0435\x0432\x044f\x0442|\x0434\x0435\x0441\x044f\x0442|\x043e\x0434\x0438\x043d\x0430\x0434\x0446\x0430\x0442|\x0434\x0432\x0435\x043d\x0430\x0434\x0446\x0430\x0442|\x0442\x0440\x0438\x043d\x0430\x0434\x0446\x0430\x0442|\x0447\x0435\x0442\x044b\x0440\x043d\x0430\x0434\x0446\x0430\x0442|\x043f\x044f\x0442\x043d\x0430\x0434\x0446\x0430\x0442|\x0448\x0435\x0441\x0442\x043d\x0430\x0434\x0446\x0430\x0442|\x0441\x0435\x043c\x043d\x0430\x0434\x0446\x0430\x0442|\x0432\x043e\x0441\x0435\x043c\x043d\x0430\x0434\x0446\x0430\x0442|\x0434\x0435\x0432\x044f\x0442\x043d\x0430\x0434\x0446\x0430\x0442|\x0434\x0432\x0430\x0434\x0446\x0430\x0442)(\x044b\x0439|\x043e\x0439|\x0438\x0439|\x0430\x044f|\x043e\x0435)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case match of
        "\x043f\x0435\x0440\x0432" -> Just $ ordinal 1
        "\x0432\x0442\x043e\x0440" -> Just $ ordinal 2
        "\x0442\x0440\x0435\x0442" -> Just $ ordinal 3
        "\x0447\x0435\x0442\x0432\x0435\x0440\x0442" -> Just $ ordinal 4
        "\x043f\x044f\x0442" -> Just $ ordinal 5
        "\x0448\x0435\x0441\x0442" -> Just $ ordinal 6
        "\x0441\x0435\x0434\x044c\x043c" -> Just $ ordinal 7
        "\x0432\x043e\x0441\x044c\x043c" -> Just $ ordinal 8
        "\x0434\x0435\x0432\x044f\x0442" -> Just $ ordinal 9
        "\x0434\x0435\x0441\x044f\x0442" -> Just $ ordinal 10
        "\x043e\x0434\x0438\x043d\x0430\x0434\x0446\x0430\x0442" -> Just $ ordinal 11
        "\x0434\x0432\x0435\x043d\x0430\x0434\x0446\x0430\x0442" -> Just $ ordinal 12
        "\x0442\x0440\x0438\x043d\x0430\x0434\x0446\x0430\x0442" -> Just $ ordinal 13
        "\x0447\x0435\x0442\x044b\x0440\x043d\x0430\x0434\x0446\x0430\x0442" -> Just $ ordinal 14
        "\x043f\x044f\x0442\x043d\x0430\x0434\x0446\x0430\x0442" -> Just $ ordinal 15
        "\x0448\x0435\x0441\x0442\x043d\x0430\x0434\x0446\x0430\x0442" -> Just $ ordinal 16
        "\x0441\x0435\x043c\x043d\x0430\x0434\x0446\x0430\x0442" -> Just $ ordinal 17
        "\x0432\x043e\x0441\x0435\x043c\x043d\x0430\x0434\x0446\x0430\x0442" -> Just $ ordinal 18
        "\x0434\x0435\x0432\x044f\x0442\x043d\x0430\x0434\x0446\x0430\x0442" -> Just $ ordinal 19
        "\x0434\x0432\x0430\x0434\x0446\x0430\x0442" -> Just $ ordinal 20
        _ -> Nothing
      _ -> Nothing
  }

ruleOrdinal :: Rule
ruleOrdinal = Rule
  { name = "ordinal 21..99"
  , pattern =
    [ regex "(\x0434\x0432\x0430\x0434\x0446\x0430\x0442\x044c|\x0442\x0440\x0438\x0434\x0446\x0430\x0442\x044c|\x0441\x043e\x0440\x043e\x043a|\x043f\x044f\x0442\x044c\x0434\x0435\x0441\x044f\x0442|\x0448\x0435\x0441\x0442\x044c\x0434\x0435\x0441\x044f\x0442|\x0441\x0435\x043c\x044c\x0434\x0435\x0441\x044f\x0442|\x0432\x043e\x0441\x0435\x043c\x044c\x0434\x0435\x0441\x044f\x0442|\x0434\x0435\x0432\x044f\x043d\x043e\x0441\x0442\x043e)"
    , regex "(\x043f\x0435\x0440\x0432|\x0432\x0442\x043e\x0440|\x0442\x0440\x0435\x0442|\x0447\x0435\x0442\x0432\x0435\x0440\x0442|\x043f\x044f\x0442|\x0448\x0435\x0441\x0442|\x0441\x0435\x0434\x044c\x043c|\x0432\x043e\x0441\x044c\x043c|\x0434\x0435\x0432\x044f\x0442)(\x044b\x0439|\x043e\x0439|\x0438\x0439|\x0430\x044f|\x043e\x0435)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:_)):
       Token RegexMatch (GroupMatch (m2:_)):
       _) -> do
         dozen <- case Text.toLower m1 of
           "\x0434\x0432\x0430\x0434\x0446\x0430\x0442\x044c" -> Just 20
           "\x0442\x0440\x0438\x0434\x0446\x0430\x0442\x044c" -> Just 30
           "\x0441\x043e\x0440\x043e\x043a" -> Just 40
           "\x043f\x044f\x0442\x044c\x0434\x0435\x0441\x044f\x0442" -> Just 50
           "\x0448\x0435\x0441\x0442\x044c\x0434\x0435\x0441\x044f\x0442" -> Just 60
           "\x0441\x0435\x043c\x044c\x0434\x0435\x0441\x044f\x0442" -> Just 70
           "\x0432\x043e\x0441\x0435\x043c\x044c\x0434\x0435\x0441\x044f\x0442" -> Just 80
           "\x0434\x0435\x0432\x044f\x043d\x043e\x0441\x0442\x043e" -> Just 90
           _ -> Nothing
         unit <- case Text.toLower m2 of
           "\x043f\x0435\x0440\x0432" -> Just 1
           "\x0432\x0442\x043e\x0440" -> Just 2
           "\x0442\x0440\x0435\x0442" -> Just 3
           "\x0447\x0435\x0442\x0432\x0435\x0440\x0442" -> Just 4
           "\x043f\x044f\x0442" -> Just 5
           "\x0448\x0435\x0441\x0442" -> Just 6
           "\x0441\x0435\x0434\x044c\x043c" -> Just 7
           "\x0432\x043e\x0441\x044c\x043c" -> Just 8
           "\x0434\x0435\x0432\x044f\x0442" -> Just 9
           _ -> Nothing
         Just . ordinal $ dozen + unit
      _ -> Nothing
  }

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern =
    [ regex "0*(\\d+)-?((\x044b|\x043e|\x0438)?\x0439|\x0430\x044f|\x043e\x0435)"
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
