-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.HU.Rules
  ( rules ) where

import Control.Monad (liftM2)
import Data.Maybe
import Data.Text (Text)
import Prelude
import Data.String
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import qualified Duckling.TimeGrain.Types as TG
import Duckling.Types
import Duckling.Time.Helpers
import Duckling.Regex.Types

instants :: [(Text, String, TG.Grain, Int)]
instants =
  [ ("right now",            "((\x00E9pp )?most)|azonnal", TG.Second, 0  )
  , ("today",                "ma",                         TG.Day,    0  )
  , ("tomorrow",             "holnap",                     TG.Day,    1  )
  , ("day after tomorrow",   "holnaput\x00E1n",            TG.Day,    2  )
  , ("yesterday",            "tegnap",                     TG.Day,    - 1)
  , ("day before yesterday", "tegnapel\x0151tt",           TG.Day,    - 2)
  , ("end of month",         "(a )?h\x00F3nap v\x00E9ge",  TG.Month,  1  )
  , ("end of year",          "(az )?\x00E9v v\x00E9ge",    TG.Year,   1  )
  ]

ruleInstants :: [Rule]
ruleInstants = map go instants
  where
    go (name, regexPattern, grain, n) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> tt $ cycleNth grain n
      }

daysOfWeek :: [(Text, String)]
daysOfWeek =
  [ ( "Monday"   , "h\x00E9tf\x0151|h\x00E9t\\.?"           )
  , ( "Tuesday"  , "kedd"                                   )
  , ( "Wednesday", "szerda|szer\\.?"                        )
  , ( "Thursday" , "cs\x00FCt\x00F6rt\x00F6k|cs\x00FCt\\.?" )
  , ( "Friday"   , "p\x00E9ntek|p\x00E9n\\.?"               )
  , ( "Saturday" , "szombat|szom\\.?"                       )
  , ( "Sunday"   , "vas\x00E1rnap|vas\\.?"                  )
  ]

ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = zipWith go daysOfWeek [1..7]
  where
    go (name, regexPattern) i = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> tt $ dayOfWeek i
      }

months :: [(Text, String)]
months =
  [ ( "January"  , "január|jan\\.?"        )
  , ( "February" , "február|febr?\\.?"     )
  , ( "March"    , "március|márc?\\.?"     )
  , ( "April"    , "április|ápr\\.?"       )
  , ( "May"      , "május|máj\\.?"         )
  , ( "June"     , "június|jún\\.?"        )
  , ( "July"     , "július|júl\\.?"        )
  , ( "August"   , "augusztus|aug\\.?"     )
  , ( "September", "szeptember|szept?\\.?" )
  , ( "October"  , "október|okt\\.?"       )
  , ( "November" , "november|nov\\.?"      )
  , ( "December" , "december|dec\\.?"      )
  ]

ruleMonths :: [Rule]
ruleMonths = zipWith go months [1..12]
  where
    go (name, regexPattern) i = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> tt $ month i
      }

ruleCycleThisLastNext :: Rule
ruleCycleThisLastNext = Rule
  { name = "this|last|next <cycle>"
  , pattern =
    [ regex "(most|el\x0151z\x0151|m\x00FAlt|k\x00F6vetkez\x0151|j\x00F6v\x0151)"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):Token TimeGrain grain:_) ->
        case Text.toLower match of
          "most"                -> tt $ cycleNth grain 0
          "el\x0151z\x0151"     -> tt . cycleNth grain $ - 1
          "m\x00FAlt"           -> tt . cycleNth grain $ - 1
          "k\x00F6vetkez\x0151" -> tt $ cycleNth grain 1
          "j\x00F6v\x0151"      -> tt $ cycleNth grain 1
          _ -> Nothing
      _ -> Nothing
  }

ruleNextDOW :: Rule
ruleNextDOW = Rule
  { name = "next <day-of-week>"
  , pattern =
    [ regex "j\x00F6v\x0151"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth 1 True td
      _ -> Nothing
  }

ruleHHMM :: Rule
ruleHHMM = Rule
  { name = "hh:mm"
  , pattern = [regex "((?:[01]?\\d)|(?:2[0-3]))[:.]([0-5]\\d)"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (hh:mm:_)):_) -> do
        h <- parseInt hh
        m <- parseInt mm
        tt $ hourMinute True h m
      _ -> Nothing
  }


ruleHHMMSS :: Rule
ruleHHMMSS = Rule
  { name = "hh:mm:ss"
  , pattern = [regex "((?:[01]?\\d)|(?:2[0-3]))[:.]([0-5]\\d)[:.]([0-5]\\d)"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (hh:mm:ss:_)):_) -> do
        h <- parseInt hh
        m <- parseInt mm
        s <- parseInt ss
        tt $ hourMinuteSecond True h m s
      _ -> Nothing
  }

ruleTODLatent :: Rule
ruleTODLatent = Rule
  { name = "time-of-day (latent)"
  , pattern =
    [ Predicate $ liftM2 (&&) isNumeralSafeToUse (isIntegerBetween 0 23)
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ hour True n
      _ -> Nothing
  }

ruleTODAM :: Rule
ruleTODAM = Rule
  { name = "am <time-of-day>"
  , pattern =
    [ regex "(de\\.?|d\x00E9lel\x0151tt)"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (ap:_)):Token Time td:_) ->
        tt . timeOfDayAMPM td $ True
      _ -> Nothing
  }

ruleTODPM :: Rule
ruleTODPM = Rule
  { name = "pm <time-of-day>"
  , pattern =
    [ regex "(du\\.?|d\x00E9lut\x00E1n)"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (ap:_)):Token Time td:_) ->
        tt . timeOfDayAMPM td $ False
      _ -> Nothing
  }

rules :: [Rule]
rules = [ ruleCycleThisLastNext
  , ruleNextDOW
  , ruleHHMM
  , ruleHHMMSS
  , ruleTODLatent
  , ruleTODAM
  , ruleTODPM
  ]
  ++ ruleInstants
  ++ ruleDaysOfWeek
  ++ ruleMonths
