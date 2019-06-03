-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Duckling.Ordinal.TA.Rules
  ( rules ) where

import Control.Monad (join)
import Data.HashMap.Strict ( HashMap)
import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Helpers
import Duckling.Regex.Types
import Duckling.Types

oneToNineMap :: HashMap Text Int
oneToNineMap = HashMap.fromList
  [ ( "முதல்", 1 )
  , ( "இரண்டாம்", 2 )
  , ( "மூன்றாம்", 3 )
  , ( "நான்காம்", 4 )
  , ( "ஐந்தாம்", 5 )
  , ( "ஆறாம்", 6 )
  , ( "ஏழாம்", 7 )
  , ( "எட்டாம்", 8 )
  , ( "ஒன்பதாம்", 9 )
  ]


ruleOneToNine :: Rule
ruleOneToNine = Rule
  { name = "integer (1..9)"
  , pattern =
    [ regex "(முதல்|இரண்டாம்|மூன்றாம்|நான்காம்|ஐந்தாம்|ஆறாம்|ஏழாம்|எட்டாம்|ஒன்பதாம்)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> HashMap.lookup (Text.toLower match) oneToNineMap
      _ -> Nothing
  }

tenToNineteenMap :: HashMap Text Int
tenToNineteenMap = HashMap.fromList
  [ ( "பத்தாம்", 10 )
  , ( "பதினொன்றாம்", 11 )
  , ( "பன்னிரண்டாம்", 12 )
  , ( "பதின்மூன்றாம்", 13 )
  , ( "பதினான்காம்", 14 )
  , ( "பதினைந்தாம்", 15 )
  , ( "பதினாறாம்", 16 )
  , ( "பதினேழாம்", 17 )
  , ( "பதினெட்டாம்", 18 )
  , ( "பத்தொன்பதாம்", 19 )
  ]

ruleTenToNineteen :: Rule
ruleTenToNineteen = Rule
  { name = "integer (10..19)"
  , pattern =
    [ regex "(பத்தாம்|பதினொன்றாம்|பன்னிரண்டாம்|பதின்மூன்றாம்|பதினான்காம்|பதினைந்தாம்|பதினாறாம்|பதினேழாம்|பதினெட்டாம்|பத்தொன்பதாம்)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> HashMap.lookup (Text.toLower match) tenToNineteenMap
      _ -> Nothing
  }

tensMap :: HashMap Text Int
tensMap = HashMap.fromList
  [ ( "இருபதாம்", 20 )
  , ( "முப்பதாம்", 30 )
  , ( "நாற்பதாம்", 40 )
  , ( "ஐம்பதாம்", 50 )
  , ( "அறுபதாம்", 60 )
  , ( "எழுபதாம்", 70 )
  , ( "எண்பதாம்", 80 )
  , ( "தொண்ணூறாம்", 90 )
  ]

ruleTens :: Rule
ruleTens = Rule
  { name = "integer (20..90)"
  , pattern =
    [ regex "(இருபதாம்|முப்பதாம்|நாற்பதாம்|ஐம்பதாம்|அறுபதாம்|எழுபதாம்|எண்பதாம்|தொண்ணூறாம்)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> HashMap.lookup (Text.toLower match) tensMap
      _ -> Nothing
  }
tensOrdinalMap :: HashMap Text Int
tensOrdinalMap = HashMap.fromList
  [ ( "இருபத்தி", 20 )
  , ( "முப்பத்து", 30 )
  , ( "நாற்பத்து", 40 )
  , ( "ஐம்பத்தி", 50 )
  , ( "அறுபத்", 60 )
  , ( "எழுபத்தி", 70 )
  , ( "எண்பத்தி", 80 )
  , ( "தொண்ணுற்று", 90 )
  ]

oneToNineOrdinalMap :: HashMap Text Int
oneToNineOrdinalMap = HashMap.fromList
  [ ( "ஒன்றாம்", 1 )
  , ( "இரண்டாம்", 2 )
  , ( "மூன்றாம்", 3 )
  , ( "நான்காம்", 4 )
  , ( "ஐந்தாம்", 5 )
  , ( "ஆறாம்", 6 )
  , ( "ஏழாம்", 7 )
  , ( "எட்டாம்", 8 )
  , ( "ஒன்பதாம்", 9 )
  ]

ruleCompositeTens :: Rule
ruleCompositeTens = Rule
  { name = "integer ([2-9][1-9])"
  , pattern =
    [ regex "(இருபத்தி|முப்பத்து|நாற்பத்து|ஐம்பத்தி|அறுபத்|எழுபத்தி|எண்பத்தி|தொண்ணுற்று)(ஒன்றாம்|இரண்டாம்|மூன்றாம்|நான்காம்|ஐந்தாம்|ஆறாம்|ஏழாம்|எட்டாம்|ஒன்பதாம்)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        v1 <- HashMap.lookup (Text.toLower m1) tensOrdinalMap
        v2 <- HashMap.lookup (Text.toLower m2) oneToNineOrdinalMap
        Just $ ordinal $ (v1 + v2)
      _ -> Nothing
  }

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern =
    [ regex "0*(\\d+)\\."
    ]
  , prod = \case
    (   Token RegexMatch (GroupMatch (match :_)) : _) -> ordinal <$> parseInt match
    _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleOrdinalDigits
  , ruleOneToNine
  , ruleTenToNineteen
  , ruleTens
  , ruleCompositeTens
  ]
