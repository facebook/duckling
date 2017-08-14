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

import Data.Maybe
import Data.Text (Text)
import Prelude
import Data.String
import qualified Data.Text as Text

import Duckling.Dimensions.Types
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

rules :: [Rule]
rules = [ ruleCycleThisLastNext]
  ++ ruleInstants
  ++ ruleDaysOfWeek
  ++ ruleMonths
