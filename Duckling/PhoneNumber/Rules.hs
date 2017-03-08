-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.PhoneNumber.Rules
  ( rules ) where

import qualified Data.Text as Text
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Number.Helpers (parseInt)
import Duckling.PhoneNumber.Types (PhoneNumberData(..))
import qualified Duckling.PhoneNumber.Types as TPhoneNumber
import Duckling.Regex.Types
import Duckling.Types

rulePhoneNumber :: Rule
rulePhoneNumber = Rule
  { name = "phone number"
  , pattern =
    [ regex $
        "(\\(?\\+(\\d{1,2})\\)?[\\s-\\.]*)?" ++ -- area code
        "((?=[-\\d()\\s\\.]{6,16}(\\s*e?xt?\\.?\\s*(\\d+))?([^\\d]+|$))([\\d(]+([-)\\s\\.]*\\d+)*)+)" ++ -- nums
        "(\\s*e?xt?\\.?\\s*(\\d+))?" -- extension
    ]
  , prod = \xs -> case xs of
      (Token RegexMatch (GroupMatch (_:code:nums:_:_:_:_:_:_:ext:_)):_) ->
        let parseNum x = toInteger <$> parseInt x
            mcode = parseNum code
            mext = parseNum ext
            cleanup = Text.filter (not . isWhitespace)
            isWhitespace x = elem x ['.', ' ', '-', '\t', '(', ')']
        in Just . Token PhoneNumber $ PhoneNumberData
          { TPhoneNumber.prefix = mcode
          , TPhoneNumber.number = cleanup nums
          , TPhoneNumber.extension = mext
          }
      _ -> Nothing
  }

rules :: [Rule]
rules = [ rulePhoneNumber ]
