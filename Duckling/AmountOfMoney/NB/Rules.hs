-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.NB.Rules
  ( rules
  ) where

import Data.Maybe
import Data.String
import Prelude

import Duckling.AmountOfMoney.Helpers
import Duckling.AmountOfMoney.Types (Currency (..), AmountOfMoneyData (..))
import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (isNatural, isPositive)
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Types
import qualified Duckling.AmountOfMoney.Types as TAmountOfMoney
import qualified Duckling.Numeral.Types as TNumeral

ruleUnitAmount :: Rule
ruleUnitAmount = Rule
  { name = "<unit> <amount>"
  , pattern =
    [ Predicate isCurrencyOnly
    , Predicate isPositive
    ]
  , prod = \case
      (Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.currency = c}:
       Token Numeral NumeralData{TNumeral.value = v}:
       _) -> Just $ Token AmountOfMoney . withValue v $ currencyOnly c
      _ -> Nothing
  }

ruleIntersectAndNumeral :: Rule
ruleIntersectAndNumeral = Rule
  { name = "intersect (and number)"
  , pattern =
    [ Predicate isWithoutCents
    , regex "og"
    , Predicate isNatural
    ]
  , prod = \case
      (Token AmountOfMoney fd:
       _:
       Token Numeral NumeralData{TNumeral.value = c}:
       _) -> Just $ Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

ruleAboutAmountofmoney :: Rule
ruleAboutAmountofmoney = Rule
  { name = "about <amount-of-money>"
  , pattern =
    [ regex "omtrent|cirka|rundt|ca"
    , Predicate isMoneyWithValue
    ]
  , prod = \case
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleCent :: Rule
ruleCent = Rule
  { name = "cent"
  , pattern =
    [ regex "cent(?:\\b|s)|\\bc\\b|pen(?:ce|ny|nies)|\\bp\\b|(?:ø|ö)rer?|fen|haleru|groszy|pais(?:e|a)\
           \|centesim(?:o|i)|centimes?|\\bct?\\b|\\brp?\\b|rap(?:pen)?s?|satang"
    ]
  , prod = const $ Just $ Token AmountOfMoney $ currencyOnly Cent
  }

ruleIntersectXCents :: Rule
ruleIntersectXCents = Rule
  { name = "intersect (X cents)"
  , pattern =
    [ Predicate isWithoutCents
    , Predicate isCents
    ]
  , prod = \case
      (Token AmountOfMoney fd:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just c}:
       _) -> Just $ Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

ruleNok :: Rule
ruleNok = Rule
  { name = "NOK"
  , pattern =
    [ regex "(?:norske? ?)?kr(?:oner?)?"
    ]
  , prod = const $ Just $ Token AmountOfMoney $ currencyOnly NOK
  }

rulePound :: Rule
rulePound = Rule
  { name = "£"
  , pattern =
    [ regex "po?und?s?"
    ]
  , prod = const $ Just $ Token AmountOfMoney $ currencyOnly Pound
  }

ruleIntersectAndXCents :: Rule
ruleIntersectAndXCents = Rule
  { name = "intersect (and X cents)"
  , pattern =
    [ Predicate isWithoutCents
    , regex "og"
    , Predicate isCents
    ]
  , prod = \case
      (Token AmountOfMoney fd:
       _:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just c}:
       _) -> Just $ Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

ruleIntersect :: Rule
ruleIntersect = Rule
  { name = "intersect"
  , pattern =
    [ Predicate isWithoutCents
    , Predicate isNatural
    ]
  , prod = \case
      (Token AmountOfMoney fd:
       Token Numeral NumeralData{TNumeral.value = c}:
       _) -> Just $ Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

ruleDirham :: Rule
ruleDirham = Rule
  { name = "AED"
  , pattern =
    [ regex "dirhams?"
    ]
  , prod = const $ Just $ Token AmountOfMoney $ currencyOnly AED
  }

ruleSEK :: Rule
ruleSEK = Rule
  { name = "SEK"
  , pattern =
    [ regex "(?:svenske? )?kron(?:a|or)|svenske? kr(?:oner?)?"
    ]
  , prod = const $ Just $ Token AmountOfMoney $ currencyOnly SEK
  }

ruleGBP :: Rule
ruleGBP = Rule
  { name = "GBP"
  , pattern =
    [ regex "(?:britiske?|engelske?) po?unds?"
    ]
  , prod = const $ Just $ Token AmountOfMoney $ currencyOnly GBP
  }

ruleDKK :: Rule
ruleDKK = Rule
  { name = "DKK"
  , pattern =
    [ regex "danske? kr(?:oner?)?"
    ]
  , prod = const $ Just $ Token AmountOfMoney $ currencyOnly DKK
  }

ruleUSD :: Rule
ruleUSD = Rule
  { name = "USD"
  , pattern =
    [ regex "amerikanske? dollars?"
    ]
  , prod = const $ Just $ Token AmountOfMoney $ currencyOnly USD
  }

ruleAUD :: Rule
ruleAUD = Rule
  { name = "AUD"
  , pattern =
    [ regex "australske? dollars?"
    ]
  , prod = const $ Just $ Token AmountOfMoney $ currencyOnly AUD
  }

ruleCAD :: Rule
ruleCAD = Rule
  { name = "CAD"
  , pattern =
    [ regex "(?:k|c)anadiske? dollars?"
    ]
  , prod = const $ Just $ Token AmountOfMoney $ currencyOnly CAD
  }

ruleCHF :: Rule
ruleCHF = Rule
  { name = "CHF"
  , pattern =
    [ regex "sveitsiske? francs?"
    ]
  , prod = const $ Just $ Token AmountOfMoney $ currencyOnly CHF
  }

ruleCNY :: Rule
ruleCNY = Rule
  { name = "CNY"
  , pattern =
    [ regex "(?:kinesiske? )?(?:yuan|renminbi)"
    ]
  , prod = const $ Just $ Token AmountOfMoney $ currencyOnly CNY
  }

ruleCZK :: Rule
ruleCZK = Rule
  { name = "CZK"
  , pattern =
    [ regex "(?:tsjekkiske? )?koruna"
    ]
  , prod = const $ Just $ Token AmountOfMoney $ currencyOnly CZK
  }

ruleHKD :: Rule
ruleHKD = Rule
  { name = "HKD"
  , pattern =
    [ regex "hong kong dollars?"
    ]
  , prod = const $ Just $ Token AmountOfMoney $ currencyOnly HKD
  }

ruleINR :: Rule
ruleINR = Rule
  { name = "INR"
  , pattern =
    [ regex "(?:indiske? )?rup(?:i(?:er)?|ees?)"
    ]
  , prod = const $ Just $ Token AmountOfMoney $ currencyOnly INR
  }

ruleJPY :: Rule
ruleJPY = Rule
  { name = "JPY"
  , pattern =
    [ regex "japanske? yen"
    ]
  , prod = const $ Just $ Token AmountOfMoney $ currencyOnly JPY
  }

ruleNZD :: Rule
ruleNZD = Rule
  { name = "NZD"
  , pattern =
    [ regex "(?:nz|new zealand(?:ske)?) dollars?"
    ]
  , prod = const $ Just $ Token AmountOfMoney $ currencyOnly NZD
  }

rulePKR :: Rule
rulePKR = Rule
  { name = "PKR"
  , pattern =
    [ regex "pakistanske? rup(?:i(?:er)?|ees?)"
    ]
  , prod = const $ Just $ Token AmountOfMoney $ currencyOnly PKR
  }

rulePLN :: Rule
rulePLN = Rule
  { name = "PLN"
  , pattern =
    [ regex "(?:polske? )?(?:z|s)?lotys?"
    ]
  , prod = const $ Just $ Token AmountOfMoney $ currencyOnly PLN
  }

ruleSGD :: Rule
ruleSGD = Rule
  { name = "SGD"
  , pattern =
    [ regex "singapor(?:e|ske?) dollars?"
    ]
  , prod = const $ Just $ Token AmountOfMoney $ currencyOnly SGD
  }

ruleTHB :: Rule
ruleTHB = Rule
  { name = "THB"
  , pattern =
    [ regex "(?:thai(?:land(?:ske?)?)? )?b(?:ah|ha)t"
    ]
  , prod = const $ Just $ Token AmountOfMoney $ currencyOnly THB
  }

ruleZAR :: Rule
ruleZAR = Rule
  { name = "ZAR"
  , pattern =
    [ regex "(?:sør(?: |-)?afrika(?:nske?)? )?rand"
    ]
  , prod = const $ Just $ Token AmountOfMoney $ currencyOnly ZAR
  }

rules :: [Rule]
rules =
  [ ruleUnitAmount
  , ruleAboutAmountofmoney
  , ruleCent
  , ruleDirham
  , ruleIntersect
  , ruleIntersectAndNumeral
  , ruleIntersectAndXCents
  , ruleIntersectXCents
  , ruleNok
  , rulePound
  , ruleSEK
  , ruleDKK
  , ruleUSD
  , ruleAUD
  , ruleCAD
  , ruleGBP
  , ruleCHF
  , ruleCNY
  , ruleCZK
  , ruleHKD
  , ruleINR
  , ruleJPY
  , ruleNZD
  , rulePKR
  , rulePLN
  , ruleSGD
  , ruleTHB
  , ruleZAR
  ]
