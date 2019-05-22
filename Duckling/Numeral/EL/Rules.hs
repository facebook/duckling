-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.EL.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.List (intercalate)
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

oneOrTwoDigitsMap :: HashMap Text Integer
oneOrTwoDigitsMap = HashMap.fromList
  [ ( "μηδέν"       , 0  )
  , ( "ένα"         , 1  )
  , ( "ένας"        , 1  )
  , ( "ενός"        , 1  )
  , ( "μία"         , 1  )
  , ( "μια"         , 1  )
  , ( "δύο"         , 2  )
  , ( "δυο"         , 2  )
  , ( "τρία"        , 3  )
  , ( "τρεις"       , 3  )
  , ( "τέσσερα"     , 4  )
  , ( "τέσσερις"    , 4  )
  , ( "πέντε"       , 5  )
  , ( "έξι"         , 6  )
  , ( "επτά"        , 7  )
  , ( "εφτά"        , 7  )
  , ( "οκτώ"        , 8  )
  , ( "οχτώ"        , 8  )
  , ( "εννιά"       , 9  )
  , ( "εννέα"       , 9  )
  , ( "δέκα"        , 10 )
  , ( "δεκαριά"     , 10 )
  , ( "έντεκα"      , 11 )
  , ( "ένδεκα"      , 11 )
  , ( "δώδεκα"      , 12 )
  , ( "ντουζίνα"    , 12 )
  , ( "ντουζίνες"   , 12 )
  , ( "δεκατρία"    , 13 )
  , ( "δεκατέσσερα" , 14 )
  , ( "δεκαπέντε"   , 15 )
  , ( "δεκαέξι"     , 16 )
  , ( "δεκαεπτά"    , 17 )
  , ( "δεκαοκτώ"    , 18 )
  , ( "δεκαεννέα"   , 19 )
  , ( "δεκαεννιά"   , 19 )
  , ( "είκοσι"      , 20 )
  , ( "τριάντα"     , 30 )
  , ( "σαράντα"     , 40 )
  , ( "πενήντα"     , 50 )
  , ( "εξήντα"      , 60 )
  , ( "εβδομήντα"   , 70 )
  , ( "ογδόντα"     , 80 )
  , ( "ενενήντα"    , 90 )
  ]

hundredsMap :: HashMap Text Integer
hundredsMap = HashMap.fromList
  [ ( "δι"   , 200 )
  , ( "τρι"  , 300 )
  , ( "τετρ" , 400 )
  , ( "πεντ" , 500 )
  , ( "εξ"   , 600 )
  , ( "επτ"  , 700 )
  , ( "εφτ"  , 700 )
  , ( "οκτ"  , 800 )
  , ( "οχτ"  , 800 )
  , ( "εννι" , 900 )
  ]

ruleNumeral :: Rule
ruleNumeral = Rule
  { name = "number (0..19, 20, 30..90)"
  , pattern = [ regex regexString ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) oneOrTwoDigitsMap >>= integer
      _ -> Nothing
  }
  where
    regexString = "(" ++ intercalate "|"
      [ "μηδέν|[εέ]ν[αοό]ς?|μ[ιί]ας?"                                -- [0..1]
      , "δ[υύ]ο|τρ(ία|εις)|τέσσερ(α|ις)|πέντε"                       -- [2..5]
      , "έξι|ε[πφ]τά|ο[κχ]τώ|ενν(ιά|έα)|δέκα|δεκαριά"                -- [6..10]
      , "έν[τδ]εκα|δώδεκα|ντουζίν(α|ες)"                             -- [11..12]
      , "δεκα(τρία|τέσσερα|πέντε|έξι|ε[πφ]τά|ο[χκ]τώ|ενν(έα|ιά))"    -- [13..19]
      , "είκοσι|(τριά|σαρά|πενή|εξή|εβδομή|ογδό|ενενή)ντα"           -- [2..9]0
      ] ++ ")"

ruleFew :: Rule
ruleFew = Rule
  { name = "few"
  , pattern =
    [ regex "μερικ(ά|ές|οί)"
    ]
  , prod = \_ -> integer 3
  }

ruleCompositeTens :: Rule
ruleCompositeTens = Rule
  { name = "integer 21..99"
  , pattern =
      [ oneOf [20,30..90]
      , numberBetween 1 10
      ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = tens} :
       Token Numeral NumeralData{TNumeral.value = units} :
       _) -> double (tens + units)
      _ -> Nothing
  }

ruleHundred :: Rule
ruleHundred = Rule
  { name = "number (100)"
  , pattern = [ regex "(εκατόν?)" ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch _):_) -> integer 100 >>= withGrain 2
      _ -> Nothing
  }

ruleHundreds :: Rule
ruleHundreds = Rule
  { name = "number (200..900)"
  , pattern =
      [ regex "(δι|τρι|τετρ|πεντ|εξ|ε(π|φ)τ|ο(χ|κ)τ|εννι)ακόσι(α|ες|οι)"
      ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) hundredsMap >>=
        integer >>= withGrain 2
      _ -> Nothing
  }

rulePowersOfTen :: Rule
rulePowersOfTen = Rule
  { name = "powers of tens"
  , pattern =
    [ regex "(χίλι(α|οι|ες)|χιλιάδες|εκατομμύρι(ο|α)|δις|δισεκατομμύριο)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "χίλια"          -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "χιλιάδες"       -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "εκατομμύριο"    -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "εκατομμύρια"    -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "δις"            -> double 1e9 >>= withGrain 9 >>= withMultipliable
        "δισεκατομμύριο" -> double 1e9 >>= withGrain 9 >>= withMultipliable
        "δισεκατομμύρια" -> double 1e9 >>= withGrain 9 >>= withMultipliable
        _                -> Nothing
      _ -> Nothing
  }

ruleNegative :: Rule
ruleNegative = Rule
  { name = "negative numbers"
  , pattern =
    [ regex "-|μείον"
    , Predicate isPositive
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral nd:_) -> double (TNumeral.value nd * (-1))
      _ -> Nothing
  }

ruleSum :: Rule
ruleSum = Rule
  { name = "intersect 2 numbers"
  , pattern =
    [ Predicate hasGrain
    , Predicate $ and . sequence [not . isMultipliable, isPositive]
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = val1, TNumeral.grain = Just g}:
       Token Numeral NumeralData{TNumeral.value = val2}:
       _) | (10 ** fromIntegral g) > val2 -> double $ val1 + val2
      _ -> Nothing
  }

ruleMultiply :: Rule
ruleMultiply = Rule
  { name = "compose by multiplication"
  , pattern =
    [ dimension Numeral
    , Predicate isMultipliable
    ]
  , prod = \tokens -> case tokens of
      (token1:token2:_) -> multiply token1 token2
      _ -> Nothing
  }

ruleDecimals :: Rule
ruleDecimals = Rule
  { name = "decimal number"
  , pattern =
    [ regex "(\\d+,\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDecimal True (Text.replace "," "." match)
      _ -> Nothing
  }

ruleCommaSpelledOut :: Rule
ruleCommaSpelledOut = Rule
  { name = "one point two"
  , pattern =
    [ dimension Numeral
    , regex "κόμμα"
    , Predicate $ not . hasGrain
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral nd1:_:Token Numeral nd2:_) ->
        double $ TNumeral.value nd1 + decimalsToDouble (TNumeral.value nd2)
      _ -> Nothing
  }

ruleDots :: Rule
ruleDots = Rule
  { name = "dot-separated numbers"
  , pattern =
    [ regex "(\\d+(\\.\\d\\d\\d)+(,\\d+)?)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDouble (
          Text.replace "," "." $ Text.replace "." Text.empty match
        ) >>= double
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleFew
  , ruleNumeral
  , ruleCompositeTens
  , rulePowersOfTen
  , ruleNegative
  , ruleHundred
  , ruleHundreds
  , ruleSum
  , ruleMultiply
  , ruleDecimals
  , ruleCommaSpelledOut
  , ruleDots
  ]
