-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.PhoneNumber.PT.Rules
  ( rules ) where

import Data.String
import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.PhoneNumber.Types (PhoneNumberData(..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.PhoneNumber.Types as TPhoneNumber

rulePhoneNumber :: Rule
rulePhoneNumber = Rule
  { name = "phone number"
  , pattern =
    -- We somewhat arbitrarly use 20 here to limit the length of matches,
    -- otherwise due to backtracking the regexp will take very long time
    -- or run out of stack for some inputs.
    [ regex $
        "(?:\\(?\\+(\\d{1,2})\\)?[\\s-\\.]*)?" ++ -- area code
        "((?=[-\\d()\\s\\.]{6,16}(?:\\s*ramal\\.?\\s*(?:\\d{1,20}))?(?:[^\\d]+|$))(?:[\\d(]{1,20}(?:[-)\\s\\.]*\\d{1,20}){0,20}){1,20})" ++ -- nums
        "(?:\\s*ramal\\.?\\s*(\\d{1,20}))?" -- extension
    ]
  , prod = \xs -> case xs of
      (Token RegexMatch (GroupMatch (code:nums:ext:_)):_) ->
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
rules =
  [ rulePhoneNumber
  ]
