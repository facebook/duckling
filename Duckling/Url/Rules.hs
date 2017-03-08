-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Url.Rules
  ( rules ) where

import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Regex.Types
import Duckling.Types
import Duckling.Url.Helpers

ruleURL :: Rule
ruleURL = Rule
  { name = "url"
  , pattern = [ regex "(([a-zA-Z]+)://)?(([\\w_-]+\\.)+[a-zA-Z]{2,4}(:\\d+)?)(/[^?\\s#]*)?(\\?[^\\s#]+)?" ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (_:protocol:domain:_:_:path:query:_)):_) ->
        Just . Token Url $ url protocol domain path query
      _ -> Nothing
  }

ruleLocalhost :: Rule
ruleLocalhost = Rule
  { name = "localhost"
  , pattern = [ regex "(([a-zA-Z]+)://)?(localhost(:\\d+)?)(/[^?\\s#]*)?(\\?[^\\s#]+)?" ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (_:protocol:domain:_:path:query:_)):_) ->
        Just . Token Url $ url protocol domain path query
      _ -> Nothing
  }

ruleLocalURL :: Rule
ruleLocalURL = Rule
  { name = "local url"
  , pattern = [ regex "([a-zA-Z]+)://([\\w_-]+(:\\d+)?)(/[^?\\s#]*)?(\\?[^\\s#]+)?" ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (protocol:domain:_:path:query:_)):_) ->
        Just . Token Url $ url protocol domain path query
      _ -> Nothing
  }

rules :: [Rule]
rules = [ ruleURL, ruleLocalhost, ruleLocalURL ]
