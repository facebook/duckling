-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


module Duckling.CreditCardNumber.Helpers
  ( otherCreditCardNumberRegex
  , visaCreditCardNumberRegex
  , amexCreditCardNumberRegex
  , discoverCreditCardNumberRegex
  , mastercardCreditCardNumberRegex
  , dinerClubCreditCardNumberRegex
  , isValidCreditCardNumber
  , minNumberDigits
  , maxNumberDigits
  , creditCard
  ) where

import Data.Text (Text)
import Prelude
import Data.String

import Duckling.CreditCardNumber.Types (CreditCardNumberData(..))
import qualified Duckling.CreditCardNumber.Types as TCreditCardNumber
import qualified Data.Text as T
import qualified Data.Char as C
import qualified Data.Bits as B

-- -----------------------------------------------------------------
-- Patterns

otherCreditCardNumberRegex :: String
otherCreditCardNumberRegex =
  concat [ "("
         , "(?!" , visaCreditCardNumberRegex, ")"
         , "(?!" , amexCreditCardNumberRegex, ")"
         , "(?!" , discoverCreditCardNumberRegex, ")"
         , "(?!" , mastercardCreditCardNumberRegex, ")"
         , "(?!" , dinerClubCreditCardNumberRegex, ")"
         , "\\d{" , show minNumberDigits , "," , show maxNumberDigits , "}"
         , ")"
         ]

-- | Visa credit card regex informed by latest BIN info
visaCreditCardNumberRegex :: String
visaCreditCardNumberRegex = "(4[0-9]{12}(?:[0-9]{3}))"

-- | American Express credit card regex informed by latest BIN info
amexCreditCardNumberRegex :: String
amexCreditCardNumberRegex = "(3[47][0-9]{13})"

-- | Discover credit card regex informed by latest BIN info
discoverCreditCardNumberRegex :: String
discoverCreditCardNumberRegex = "(6(?:011|5[0-9]{2})[0-9]{12})"

-- | Mastercard credit card regex informed by latest BIN info
mastercardCreditCardNumberRegex :: String
mastercardCreditCardNumberRegex = "(5[1-5][0-9]{14})"

-- | Diner Club credit card regex informed by latest BIN info
dinerClubCreditCardNumberRegex :: String
dinerClubCreditCardNumberRegex = "(3(?:0[0-5]|[68][0-9])[0-9]{11})"

-- -----------------------------------------------------------------
-- Validation

-- | An implementation of the Luhn algorithm (see
-- https://en.wikipedia.org/wiki/Luhn_algorithm) to check if a given credit card
-- number is valid
isValidCreditCardNumber :: Text -> Bool
isValidCreditCardNumber ccNum =
  T.length ccNum >= minNumberDigits &&
  T.length ccNum <= maxNumberDigits &&
  validCheckSum
  where
  validCheckSum :: Bool
  validCheckSum =
    T.all C.isDigit ccNum &&
    fst (T.foldr f (0, 0) ccNum) `rem` 10 == 0
    where
    f char (checksum, e) =
      let
        val = C.digitToInt char
        -- every even digit should be doubled
        d = sumDigits (B.shift val e)
      in (checksum + d, 1 - e)
    -- we only need sum of digits for numbers from 0 to 18
    sumDigits a
      | a > 9 = a - 9
      | otherwise = a

minNumberDigits :: Int
minNumberDigits = 8

maxNumberDigits :: Int
maxNumberDigits = 19

-- -----------------------------------------------------------------
-- Production

creditCard :: Text -> TCreditCardNumber.Issuer -> CreditCardNumberData
creditCard ccNum i =
  CreditCardNumberData { TCreditCardNumber.number = ccNum
                       , TCreditCardNumber.issuer = i
                       }
