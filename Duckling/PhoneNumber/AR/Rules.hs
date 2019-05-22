-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Duckling.PhoneNumber.AR.Rules (rules) where

import Duckling.Dimensions.Types
import Duckling.Numeral.AR.Helpers
  ( parseArabicIntAsText
  , parseArabicIntegerFromText
  )
import Duckling.PhoneNumber.Types (PhoneNumberData(..))
import Duckling.Regex.Types
import Duckling.Types
import Prelude

import qualified Data.Text as Text
import qualified Duckling.PhoneNumber.Types as TPhoneNumber

rulePhoneNumber :: Rule
rulePhoneNumber = Rule
  { name = "phone number"
  , pattern =
    -- Arabic is a right to left langauge except for numbers, which are read
    -- left to right. This regex uses the unicode range for Arabic numbers
    -- [\1632-\1641] to make the code easier to read and maintain. The unicode
    -- sequence \1601\1585\1593\1610, corresponding to فرعي, is a popular
    -- Arabic equivalent for "extension" and is used in this regex.
    [ regex $
        "(?:\\(?\\+([\1632-\1641]{1,2})\\)?[\\s-\\.]*)?" ++ -- area code
        "((?=[-\1632-\1641()\\s\\.]{6,16}(?:\\s*(?:e?xt?|\1601\1585\1593\1610)?\\.?\\s*(?:[\1632-\1641]{1,20}))?(?:[^\1632-\1641]+|$))(?:[\1632-\1641(]{1,20}(?:[-)\\s\\.]*[\1632-\1641]{1,20}){0,20}){1,20})" ++ -- nums
        "(?:\\s*(?:e?xt?|\1601\1585\1593\1610)\\.?\\s*([\1632-\1641]{1,20}))?" -- extension
    ]
  , prod = \xs -> case xs of
      (Token RegexMatch (GroupMatch (code:nums:ext:_)):_) ->
        let
            mnums = parseArabicIntAsText $ cleanup nums
            cleanup = Text.filter (not . isWhitespace)
            isWhitespace x = elem x ['.', ' ', '-', '\t', '(', ')']
        in Just $ Token PhoneNumber $ PhoneNumberData
          { TPhoneNumber.prefix = parseArabicIntegerFromText code
          , TPhoneNumber.number = mnums
          , TPhoneNumber.extension = parseArabicIntegerFromText ext
          }
      _ -> Nothing
  }

rules :: [Rule]
rules = [rulePhoneNumber]
