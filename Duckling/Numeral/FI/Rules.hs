{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Numeral.FI.Rules
  ( rules ) where

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

ruleIntegerNumeric :: Rule
ruleIntegerNumeric = Rule
  { name = "integer (numeric)"
  , pattern =
    [ regex "(\\d{1,18})"
    ]
  , prod = \tokens ->
      case tokens of
        (Token RegexMatch (GroupMatch (match:_)):_) -> do
          v <- parseInt match
          integer $ toInteger v
        _ -> Nothing
  }

numeralMap :: HashMap Text Integer
numeralMap = HashMap.fromList
  [ ( "nolla", 0 )
  , ( "yksi", 1 )
  , ( "kaksi", 2 )
  , ( "kolme", 3 )
  , ( "nelj\x00E4", 4 )
  , ( "viisi", 5 )
  , ( "kuusi", 6 )
  , ( "seitsem\x00E4n", 7 )
  , ( "kahdeksan", 8 )
  , ( "yhdeks\x00E4n", 9 )
  , ( "kymmenen", 10 )
  ]

ruleNumeral :: Rule
ruleNumeral = Rule
  { name = "number (0..10)"
  , pattern =
    [ regex "(nolla|yksi|kaksi|kolme|nelj\x00E4|viisi|kuusi|seitsem\x00E4n|kahdeksan|yhdeks\x00E4n|kymmenen)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) numeralMap >>= integer
      _ -> Nothing
  }

elevenToNineteenMap :: HashMap Text Integer
elevenToNineteenMap = HashMap.fromList
  [ ( "yksitoista", 11 )
  , ( "kaksitoista", 12 )
  , ( "kolmetoista", 13 )
  , ( "nelj\x00E4toista", 14 )
  , ( "viisitoista", 15 )
  , ( "kuusitoista", 16 )
  , ( "seitsem\x00E4ntoista", 17 )
  , ( "kahdeksantoista", 18 )
  , ( "yhdeks\x00E4ntoista", 19 )
  ]

ruleElevenToNineteen :: Rule
ruleElevenToNineteen = Rule
  { name = "number (11..19)"
  , pattern =
    [ regex "(yksitoista|kaksitoista|kolmetoista|nelj\x00E4toista|viisitoista|kuusitoista|seitsem\x00E4ntoista|kahdeksantoista|yhdeks\x00E4ntoista)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) elevenToNineteenMap >>= integer
      _ -> Nothing
  }

tensMap :: HashMap Text Integer
tensMap = HashMap.fromList
  [ ( "kaksikymment\x00E4", 20 )
  , ( "kolmekymment\x00E4", 30 )
  , ( "nelj\x00E4kymment\x00E4", 40 )
  , ( "viisikymment\x00E4", 50 )
  , ( "kuusikymment\x00E4", 60 )
  , ( "seitsem\x00E4nkymment\x00E4", 70 )
  , ( "kahdeksankymment\x00E4", 80 )
  , ( "yhdeks\x00E4nkymment\x00E4", 90 )
  ]

ruleTens :: Rule
ruleTens = Rule
  { name = "integer (20,30..90)"
  , pattern =
    [ regex "(kaksikymment\x00E4|kolmekymment\x00E4|nelj\x00E4kymment\x00E4|viisikymment\x00E4|kuusikymment\x00E4|seitsem\x00E4nkymment\x00E4|kahdeksankymment\x00E4|yhdeks\x00E4nkymment\x00E4)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) tensMap >>= integer
      _ -> Nothing
  }

ruleCompositeTens :: Rule
ruleCompositeTens = Rule
  { name = "integer ([2-9][1-9])"
  , pattern =
    [ regex "(kaksikymment\x00E4|kolmekymment\x00E4|nelj\x00E4kymment\x00E4|viisikymment\x00E4|kuusikymment\x00E4|seitsem\x00E4nkymment\x00E4|kahdeksankymment\x00E4|yhdeks\x00E4nkymment\x00E4)(yksi|kaksi|kolme|nelj\x00E4|viisi|kuusi|seitsem\x00E4n|kahdeksan|yhdeks\x00E4n)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        v1 <- HashMap.lookup (Text.toLower m1) tensMap
        v2 <- HashMap.lookup (Text.toLower m2) numeralMap
        integer $ v1 + v2
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleIntegerNumeric
  , ruleNumeral
  , ruleElevenToNineteen
  , ruleTens
  , ruleCompositeTens
  ]
