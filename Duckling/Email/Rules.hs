-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Email.Rules
  ( rules ) where

import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Email.Types (EmailData (..))
import qualified Duckling.Email.Types as TEmail
import Duckling.Regex.Types
import Duckling.Types

ruleEmail :: Rule
ruleEmail = Rule
  { name = "email"
  , pattern =
    [ regex "([\\w\\._+-]+@[\\w_-]+(\\.[\\w_-]+)+)"
    ]
  , prod = \xs -> case xs of
      (Token RegexMatch (GroupMatch (x:_)):_) ->
        Just $ Token Email EmailData {TEmail.value = x}
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleEmail
  ]
