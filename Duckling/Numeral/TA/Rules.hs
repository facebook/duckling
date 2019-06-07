-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE LambdaCase #-}

module Duckling.Numeral.TA.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral

zeroToNineMap :: HashMap Text Integer
zeroToNineMap = HashMap.fromList
  [ ( "பூஜ்ஜியம்", 0 )
  , ( "ஒன்று", 1 )
  , ( "இரண்டு", 2 )
  , ( "மூன்று", 3 )
  , ( "நான்கு", 4 )
  , ( "ஐந்து", 5 )
  , ( "ஆறு", 6 )
  , ( "ஏழு", 7 )
  , ( "எட்டு", 8 )
  , ( "ஒன்பது", 9 )
  ]

ruleZeroToNine :: Rule
ruleZeroToNine = Rule
  { name = "integer (0..9)"
  , pattern =
    [ regex "(பூஜ்ஜியம்|ஒன்று|இரண்டு|மூன்று|நான்கு|ஐந்து|ஆறு|ஏழு|எட்டு|ஒன்பது)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) zeroToNineMap >>= integer
      _ -> Nothing
  }

tenToNineteenMap :: HashMap Text Integer
tenToNineteenMap = HashMap.fromList
  [ ( "பத்து", 10 )
  , ( "பதினொன்று", 11 )
  , ( "பன்னிரண்டு", 12 )
  , ( "பதின்மூன்று", 13 )
  , ( "பதினான்கு", 14 )
  , ( "பதினைந்து", 15 )
  , ( "பதினாறு", 16 )
  , ( "பதினேழு", 17 )
  , ( "பதினெட்டு", 18 )
  , ( "பத்தொன்பது", 19 )
  ]

ruleTenToNineteen :: Rule
ruleTenToNineteen = Rule
  { name = "integer (10..19)"
  , pattern =
    [ regex "(பத்து|பதினொன்று|பன்னிரண்டு|பதின்மூன்று|பதினான்கு|பதினைந்து|பதினாறு|பதினேழு|பதினெட்டு|பத்தொன்பது)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) tenToNineteenMap >>= integer
      _ -> Nothing
  }

tensMap :: HashMap Text Integer
tensMap = HashMap.fromList
  [ ( "இருபது", 20 )
  , ( "இருபத்தி", 20 )
  , ( "முப்பது", 30 )
  , ( "முப்பத்து", 30 )
  , ( "நாற்பது", 40 )
  , ( "நாற்பத்து", 40 )
  , ( "ஐம்பது", 50 )
  , ( "ஐம்பத்தி", 50 )
  , ( "அறுபது", 60 )
  , ( "அறுபத்", 60 )
  , ( "எழுபது", 70 )
  , ( "எழுபத்தி", 70 )
  , ( "எண்பது", 80 )
  , ( "எண்பத்", 80 )
  , ( "தொண்ணூறு", 90 )
  , ( "தொண்ணுற்று", 90 )
  ]

ruleTens :: Rule
ruleTens = Rule
  { name = "integer (20..90)"
  , pattern =
    [ regex "(இருபது|முப்பது|நாற்பது|ஐம்பது|அறுபது|எழுபது|எண்பது|தொண்ணூறு)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) tensMap >>= integer
      _ -> Nothing
  }

ruleCompositeTens :: Rule
ruleCompositeTens = Rule
  { name = "integer ([2-9][1-9])"
  , pattern =
    [ regex "(இருபத்தி|முப்பத்து|நாற்பத்து|ஐம்பத்தி|அறுபத்|எழுபத்தி|எண்பத்|தொண்ணுற்று)(ஒன்று|இரண்டு|மூன்று|நான்கு|ஐந்து|ஆறு|ஏழு|எட்டு|ஒன்பது)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        v1 <- HashMap.lookup (Text.toLower m1) tensMap
        v2 <- HashMap.lookup (Text.toLower m2) zeroToNineMap
        integer $ v1 + v2
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleZeroToNine
  , ruleTenToNineteen
  , ruleTens
  , ruleCompositeTens
  ]
