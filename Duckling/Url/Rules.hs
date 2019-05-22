-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


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
  , pattern =
    [ regex "((([a-zA-Z]+)://)?(w{2,3}[0-9]*\\.)?(([\\w_-]+\\.)+[a-z]{2,4})(:(\\d+))?(/[^?\\s#]*)?(\\?[^\\s#]+)?(#[\\-,*=&a-z0-9]+)?)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m:_:_protocol:_:domain:_:_:_port:_path:_query:_)):
       _) -> Just . Token Url $ url m domain
      _ -> Nothing
  }

ruleLocalhost :: Rule
ruleLocalhost = Rule
  { name = "localhost"
  , pattern =
    [ regex "((([a-zA-Z]+)://)?localhost(:(\\d+))?(/[^?\\s#]*)?(\\?[^\\s#]+)?)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m:_:_protocol:_:_port:_path:_query:_)):_) ->
        Just . Token Url $ url m "localhost"
      _ -> Nothing
  }

ruleLocalURL :: Rule
ruleLocalURL = Rule
  { name = "local url"
  , pattern =
    [ regex "(([a-zA-Z]+)://([\\w_-]+)(:(\\d+))?(/[^?\\s#]*)?(\\?[^\\s#]+)?)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m:_protocol:domain:_:_port:_path:_query:_)):
       _) -> Just . Token Url $ url m domain
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleURL
  , ruleLocalhost
  , ruleLocalURL
  ]
