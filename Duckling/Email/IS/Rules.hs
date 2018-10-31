-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Email.IS.Rules
  ( rules ) where

import Data.String
import qualified Data.Text as Text
import Prelude

import Duckling.Dimensions.Types
import Duckling.Email.Types (EmailData (..))
import qualified Duckling.Email.Types as TEmail
import Duckling.Regex.Types
import Duckling.Types

ruleEmailSpelledOut :: Rule
ruleEmailSpelledOut = Rule
  { name = "email spelled out"
  , pattern =
    [ regex "([\\w\\._+-]+) ingi ([\\w_-]+(\\.[\\w_-]+)+)"
    ]
  , prod = \xs -> case xs of
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) ->
        Just $ Token Email EmailData {TEmail.value = Text.concat [m1, "@", m2]}
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleEmailSpelledOut
  ]
