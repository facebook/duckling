-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.CreditCardNumber.Rules
  ( rules ) where

import Prelude
import Data.String
import Data.Text (Text)
import Data.Bool

import Duckling.Dimensions.Types
import Duckling.CreditCardNumber.Helpers
import qualified Duckling.CreditCardNumber.Types as TCreditCardNumber
import Duckling.Regex.Types
import Duckling.Types

creditCards :: [(Text, PatternItem, TCreditCardNumber.Issuer)]
creditCards =
  [ ( "visa credit card number"
    , regex visaCreditCardNumberRegex
    , TCreditCardNumber.Visa
    )
  , ( "amex card number"
    , regex amexCreditCardNumberRegex
    , TCreditCardNumber.Amex
    )
  , ( "discover card number"
    , regex discoverCreditCardNumberRegex
    , TCreditCardNumber.Discover
    )
  , ( "mastercard card number"
    , regex mastercardCreditCardNumberRegex
    , TCreditCardNumber.Mastercard
    )
  , ( "diner club card number"
    , regex dinerClubCreditCardNumberRegex
    , TCreditCardNumber.DinerClub
    )
  , ( "credit card number"
    , regex otherCreditCardNumberRegex
    , TCreditCardNumber.Other
    )
  ]

rules :: [Rule]
rules = map go creditCards
  where
    go :: (Text, PatternItem, TCreditCardNumber.Issuer) -> Rule
    go (name, regexPattern, i) = Rule
      { name = name
      , pattern = [ regexPattern ]
      , prod = \case
          (Token RegexMatch (GroupMatch (ccNum:_)):_) ->
            bool
              Nothing
              (Just $ Token CreditCardNumber $ creditCard ccNum i)
              (isValidCreditCardNumber ccNum)
          _ -> Nothing
      }
