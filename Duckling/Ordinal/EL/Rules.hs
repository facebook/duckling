-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.EL.Rules
  ( rules ) where

import Data.HashMap.Strict ( HashMap)
import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Helpers
import Duckling.Ordinal.Types (OrdinalData (..), isBetween)
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Ordinal.Types as TOrdinal

ordinalsMap :: HashMap Text Int
ordinalsMap = HashMap.fromList
  [ ( "πρώτ"        , 1  )
  , ( "δεύτερ"      , 2  )
  , ( "δευτέρ"      , 2  )
  , ( "τρίτ"        , 3  )
  , ( "τέταρτ"      , 4  )
  , ( "τετάρτ"      , 4  )
  , ( "πέμπτ"       , 5  )
  , ( "έκτ"         , 6  )
  , ( "έβδομ"       , 7  )
  , ( "εβδόμ"       , 7  )
  , ( "όγδο"        , 8  )
  , ( "ογδό"        , 8  )
  , ( "ένατ"        , 9  )
  , ( "ενάτ"        , 9  )
  , ( "δέκατ"       , 10 )
  , ( "δεκάτ"       , 10 )
  , ( "ενδέκατ"     , 11 )
  , ( "ενδεκάτ"     , 11 )
  , ( "δωδέκατ"     , 12 )
  , ( "δωδεκάτ"     , 12 )
  , ( "εικοστ"      , 20 )
  , ( "τριακοστ"    , 30 )
  , ( "τεσσαρακοστ" , 40 )
  , ( "πεντηκοστ"   , 50 )
  , ( "εξηκοστ"     , 60 )
  , ( "εβδομηκοστ"  , 70 )
  , ( "ογδοηκοστ"   , 80 )
  , ( "ενενηκοστ"   , 90 )
  ]

ruleOrdinals :: Rule
ruleOrdinals = Rule
  { name = "ordinals (1st..12th, 20th, 30th..90th)"
  , pattern =
      [ regex $ "(πρώτ|δε[υύ]τ[εέ]ρ|τρίτ|τ[εέ]τ[αά]ρτ|πέμπτ|"
             ++ "έκτ|[εέ]βδ[οό]μ(ηκοστ)?|[οό]γδ[οό](ηκοστ)?|[εέ]ν[αά]τ|δ[εέ]κ[αά]τ|"
             ++ "εν[δτ][εέ]κ[αά]τ|δωδ[εέ]κ[αά]τ|"
             ++ "εικοστ|τριακοστ|τεσσαρακοστ|πεντηκοστ|"
             ++ "εξηκοστ|ενενηκοστ)"
             ++ "([οό][υύιί]?ς?|[ηή]ς?|[εέ]ς|ων)"
      ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> HashMap.lookup (Text.toLower match) ordinalsMap
      _ -> Nothing
    }

ruleCompositeOrdinals :: Rule
ruleCompositeOrdinals = Rule
  { name = "ordinals (composite: 11th..19th, 21st..29th, ..., 91st..99th)"
  , pattern =
      [ oneOf [10..90]
      , oneOf [1..9]
      ]
  , prod = \tokens -> case tokens of
      ( Token Ordinal OrdinalData{TOrdinal.value = t} :
        Token Ordinal OrdinalData{TOrdinal.value = u} : _ )
        -> Just $ ordinal $ t + u
      _ -> Nothing
  }

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern = [regex "0*(\\d+) ?(ο[ςυι]?|ης?|ες)"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> ordinal <$> parseInt match
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleOrdinals
  , ruleCompositeOrdinals
  , ruleOrdinalDigits
  ]
