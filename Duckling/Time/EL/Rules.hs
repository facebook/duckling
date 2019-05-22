-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.EL.Rules
  ( rules ) where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers (duration)
import Duckling.Duration.Types (DurationData (DurationData))
import Duckling.Numeral.Helpers (parseInt, numeralMapEL)
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Ordinal.Types (OrdinalData (..), isBetween)
import Duckling.Regex.Types
import Duckling.Time.Helpers
import Duckling.Time.Types (TimeData (..))
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.Ordinal.Types as TOrdinal
import qualified Duckling.Time.Types as TTime
import qualified Duckling.TimeGrain.Types as TG

ruleInstants :: [Rule]
ruleInstants = mkRuleInstants
  [ ( "now"             , TG.Second,  0, "(αμέσως\\s+)?τώρα(\\s+αμέσως)?|αυτή τη στιγμή" )
  , ( "today"           , TG.Day   ,  0, "σήμερα"                   )
  , ( "tomorrow"        , TG.Day   ,  1, "(επ)?αύριο"               )
  , ( "yesterday"       , TG.Day   , -1, "ε?χ[θτ][εέ]ς"             )
  , ( "after tomorrow"  , TG.Day   ,  2, "μεθαύριο"                 )
  , ( "before yesterday", TG.Day   , -2, "προχ[θτ]ές"               )
  , ( "EOD|End of day"  , TG.Day   ,  1, "τέλου?ς\\s+της\\s+η?μέρας")
  , ( "EOM|End of month", TG.Month ,  1, "τέλου?ς\\s+του\\s+μήνα"   )
  , ( "EOY|End of year" , TG.Year  ,  1, "τέλου?ς\\s+του\\s+χρόνου" )
  ]

daysOfWeek :: [(Text, String)]
daysOfWeek =
  [ ( "monday"    , "δευτ(έρας?|\\.?)"          )
  , ( "tuesday"   , "τρ[ιί](της?|\\.?)"         )
  , ( "wednesday" , "τετ(άρτης?|\\.?)"          )
  , ( "thursday"  , "π[εέ]μ(πτης?|\\.?)"        )
  , ( "friday"    , "παρ(ασκευής?|\\.?)"        )
  , ( "saturday"  , "σ[αά]β(β[αά]το[νυ]?|\\.?)" )
  , ( "sunday"    , "κυρ(ιακής?|\\.?)"          )
  ]

ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = zipWith go daysOfWeek [1..7]
  where
    go (name, regexPattern) i = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = const . tt . mkOkForThisNext $ dayOfWeek i
      }

months :: [(Text, String)]
months =
  [ ( "January"  , "ιαν(ου[αά]ρ[ιί]ο[υς]?)?|γενάρης?"    )
  , ( "February" , "φεβ(ρου[αά]ρ[ιί]ο[υς]?)?|φλεβάρης?"  )
  , ( "March"    , "μ[αά]ρ(τ([ιί]ο([νυ]?)|η)ς?)?"        )
  , ( "April"    , "απρ([ιί]λ([ιί]ο([νυ]?)|η)ς?)?"       )
  , ( "May"      , "μ[αά]([ιίϊΐ]ο[νυ]?|η)ς?"             )
  , ( "June"     , "ιο[υύ]ν([ιί]ο[υν]?|η)?ς?"            )
  , ( "July"     , "ιο[υύ]λ([ιί]ο[υν]?|η)?ς?"            )
  , ( "August"   , "α[υύ]γ(ο[υύ]στο(ν|υ|ς)?)?"           )
  , ( "September", "σεπτ([εέ]μβρ([ιί]ο([νυ]?)|η)ς?)?"    )
  , ( "October"  , "οκτ([ωώ]βρ([ιί]ο([νυ]?)|η)ς?)?"      )
  , ( "November" , "νο[εέ](μ(βρ([ιί]ο([νυ]?)|η)ς?)?)?"   )
  , ( "December" , "δεκ([εέ]μβρ([ιί]ο([νυ]?)|η)ς?)?"     )
  ]

ruleMonths :: [Rule]
ruleMonths = zipWith go months [1..12]
  where
    go (name, regexPattern) i = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = const . tt . mkOkForThisNext $ month i
      }

ruleSeasons :: [Rule]
ruleSeasons = mkRuleSeasons
  [ ( "summer" , "καλοκα[ιί]ρι(ού)?", monthDay  6 21, monthDay  9 23 )
  , ( "fall"   , "φθιν[οό]π[ωώ]ρου?", monthDay  9 23, monthDay 12 21 )
  , ( "winter" , "χειμώνας?"        , monthDay 12 21, monthDay  3 20 )
  , ( "spring" , "άνοιξης?"         , monthDay  3 20, monthDay  6 21 )
  ]

ruleHolidays :: [Rule]
ruleHolidays = map go holidays
  where
    go (name, td, regexPattern) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = const . tt $ mkOkForThisNext td
      }

holidays :: [(Text, TimeData, String)]
holidays =
  [ ("new year's day"   , monthDay 1 1, "πρωτοχρονιάς?")
  , ("valentine's day"  , monthDay 2 14, "αγίου\\s+βαλεντίνου")
  , ("halloween day"    , monthDay 10 31, "halloween")
  , ("Epiphany"         , monthDay 1 6, "θεοφ(αά)νε[ιί](α|ων)|φ[ωώ]τ(α|ων)")
  , ("annunciation day" , monthDay 3 25, "ευαγγελισμ(ός|ού)\\s+της\\s+θεοτόκου")
  , ("revolution day"   , monthDay 3 25
                        , "η?μέρα\\s+(της\\s+)?(ελληνικής\\s+)?επανάστασης")
  , ("assumption day"   , monthDay  8 15
                        , "κο[ιί]μ[ηή]σ(η|ις|εως)\\s+της\\s+θεοτόκου")
  , ("christmas eve"    , monthDay 12 24, "παραμν(ή|έ)ς?\\s+χριστουγέννων")
  , ("christmas"        , monthDay 12 25, "χριστο[υύ]γ[εέ]νν(α|ων)")
  , ("new year's eve"   , monthDay 12 31, "παραμον(ή|έ)ς?\\s+πρωτοχρονιάς")
  , ("Mother's Day"     , nthDOWOfMonth 2 7 5, "η?μέρας?\\s+της\\s+μητέρας")
  , ("Father's Day"     , nthDOWOfMonth 3 7 6
                        , "(γιορτής?|η?μέρας?)\\s+του\\s+πατέρα")
  ]

ruleRelativeIntegerToOrAfterIntegerPartOfDay :: Rule
ruleRelativeIntegerToOrAfterIntegerPartOfDay = Rule
  { name = "relative integer (minutes) to|till|before|after <integer> (time-of-day)"
  , pattern =
    [ Predicate $ isIntegerBetween 1 30
    , regex "(πριν|μετά)"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:
       Token RegexMatch (GroupMatch (match:_)):
       Token Time td:
       _) -> case Text.toLower match of
        "πριν" -> tt $ durationBefore (duration TG.Minute $ floor v) td
        _      -> tt $ durationAfter (duration TG.Minute $ floor v) td
      _ -> Nothing
  }

ruleQuarterBeforeOrAfterIntegerHourofday :: Rule
ruleQuarterBeforeOrAfterIntegerHourofday = Rule
  { name = "quarter to|past <integer> (hour-of-day)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "(παρά|και)\\s+τέταρτο"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
        "παρά" -> Token Time <$> minutesBefore 15 td
        _      -> Token Time <$> minutesAfter  15 td
      _ -> Nothing
  }

ruleHalfAfterIntegerHourofday :: Rule
ruleHalfAfterIntegerHourofday = Rule
  { name = "half after|past <integer> (hour-of-day)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "και μισή"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _) -> tt $ hourMinute is12H hours 30
      _ -> Nothing
  }

ruleHalfAfterIntegerHourofday2 :: Rule
ruleHalfAfterIntegerHourofday2 = Rule
  { name = "<integer>-and-half (hour-of-day)"
  , pattern =
    [ regex $ "(μιά|ενά|δυό|τρεισή|τεσσερι?σή|πεντέ|εξί|ε[πφ]τά|ο[κχ]τώ|εννιά|"
           ++ "δεκά|εντεκά|δωδεκά)μισ[ιη]ς?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (num:_)):_) ->
        case HashMap.lookup (Text.toLower num) numeralMapEL of
          Just hours -> tt $ hourMinute True hours 30
          _ -> Nothing
      _ -> Nothing
  }

ruleOrdinalCycleOfTime :: Rule
ruleOrdinalCycleOfTime = Rule
  { name = "<ordinal> <cycle> of <time>"
  , pattern =
    [ dimension Ordinal
    , dimension TimeGrain
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal od:Token TimeGrain grain:Token Time td:_) ->
        tt $ cycleNthAfter True grain (TOrdinal.value od - 1) td
      _ -> Nothing
  }

ruleLastTime :: Rule
ruleLastTime = Rule
  { name = "last <time>"
  , pattern =
    [ regex "(τελευταί|περασμέν|προηγο[υύ]μ[εέ]ν)(α|ά|ο[υύ]?|η|ή|ε|έ|ω|ώ)ν?ς?"
    , Predicate isOkWithThisNext
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth (-1) False td
      _ -> Nothing
  }

ruleDatetimeDatetimeInterval :: Rule
ruleDatetimeDatetimeInterval = Rule
  { name = "<datetime> - <datetime> (interval)"
  , pattern =
    [ Predicate isNotLatent
    , regex "\\-|μ[εέ](χρι)?(\\s+τις)?"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time . notLatent <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleEvening :: Rule
ruleEvening = Rule
  { name = "evening"
  , pattern =
    [ regex "βράδυ"
    ]
  , prod = \_ ->
      let from = hour False 18
          to = hour False 0
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleTheDayofmonthNonOrdinal :: Rule
ruleTheDayofmonthNonOrdinal = Rule
  { name = "the <day-of-month> (non ordinal)"
  , pattern =
    [ regex "τ?η[νς]?"
    , Predicate $ isIntegerBetween 1 31
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Token Time . dayOfMonth <$> getIntValue token
      _ -> Nothing
  }

rulePartOfMonth :: Rule
rulePartOfMonth = Rule
  { name = "part of <named-month>"
  , pattern =
    [ regex "(αρχ(?:ές|η)|μέσ[οα]υ?|τέλ(?:ου?ς|η))(?:\\s+του)?"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):Token Time td:_) -> do
        (sd, ed) <- case Text.take 3 $ Text.toLower match of
          "αρχ" -> Just (1, 10)
          "μέσ" -> Just (11, 20)
          "τέλ" -> Just (21, -1)
          _     -> Nothing
        start <- intersect td $ dayOfMonth sd
        end <- if ed /= -1
          then intersect td $ dayOfMonth ed
          else Just $ cycleLastOf TG.Day td
        Token Time <$> interval TTime.Open start end
      _ -> Nothing
  }

ruleInDuration :: Rule
ruleInDuration = Rule
  { name = "in <duration>"
  , pattern =
    [ regex "σε"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) -> tt $ inDuration dd
      _ -> Nothing
  }

ruleLastCycleOfTime :: Rule
ruleLastCycleOfTime = Rule
  { name = "last <cycle> of <time>"
  , pattern =
    [ regex "τελευταί(ο[ιυ]?ς?|α|ες|ων)"
    , dimension TimeGrain
    , regex "σ?τ(ο[υν]?|η[νς]?|ων)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleLastOf grain td
      _ -> Nothing
  }

ruleLastCycleOfTimeS :: Rule
ruleLastCycleOfTimeS = Rule
  { name = "last <cycle> <time>'s'"
  , pattern =
    [ regex "τελευταί(ο[ιυ]?ς?|α|ες|ων)"
    , dimension TimeGrain
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:Token Time td:_) ->
        tt $ cycleLastOf grain td
      _ -> Nothing
  }

ruleFromDatetimeDatetimeInterval :: Rule
ruleFromDatetimeDatetimeInterval = Rule
  { name = "from <datetime> - <datetime> (interval)"
  , pattern =
    [ regex "από(\\s+τ(ις|η))?"
    , Predicate $ and . sequence [isNotLatent, isATimeOfDay]
    , regex "\\-|μέχρι(\\s+τ(ις|η))?"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

-- Specific for time-of-day, to help resolve ambiguities
ruleIntervalTODDash :: Rule
ruleIntervalTODDash = Rule
  { name = "<time-of-day> - <time-of-day> (interval)"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isATimeOfDay]
    , regex "\\-|μέχρι"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleMonthDdddInterval :: Rule
ruleMonthDdddInterval = Rule
  { name = "dd-dd <month> (interval)"
  , pattern =
    [ regex "(από\\s+)?(τ[ιη][νς]?\\s+)?([012]?\\d|30|31)η?ς?"
    , regex "\\-|μ[εέ](χρι)?(?:\\s+τ[ιη][νς]?)?"
    , regex "([012]?\\d|30|31)η?ς?"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (_:_:m1:_)):
       _:
       Token RegexMatch (GroupMatch (m2:_)):
       Token Time td:
       _) -> do
        v1 <- parseInt m1
        v2 <- parseInt m2
        from <- intersect (dayOfMonth v1) td
        to <- intersect (dayOfMonth v2) td
        Token Time <$> interval TTime.Closed from to
      _ -> Nothing
  }

ruleTheCycleAfterTime :: Rule
ruleTheCycleAfterTime = Rule
  { name = "the <cycle> after <time>"
  , pattern =
    [ dimension TimeGrain
    , regex "μετά τ(ου?|η|ι|α|ου)ν?ς?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter False grain 1 td
      _ -> Nothing
  }

ruleTheCycleOfTime :: Rule
ruleTheCycleOfTime = Rule
  { name = "the <cycle> of <time>"
  , pattern =
    [ dimension TimeGrain
    , regex "τ(ου|ης)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter True grain 0 td
      _ -> Nothing
  }

ruleTheCycleBeforeTime :: Rule
ruleTheCycleBeforeTime = Rule
  { name = "the <cycle> before <time>"
  , pattern =
    [ dimension TimeGrain
    , regex "πριν τ(ον?|ην?|α|ους)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter False grain (-1) td
      _ -> Nothing
  }

ruleYearLatent2 :: Rule
ruleYearLatent2 = Rule
  { name = "year (latent)"
  , pattern =
    [ Predicate $ isIntegerBetween 2101 10000
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        v <- getIntValue token
        tt . mkLatent $ year v
      _ -> Nothing
  }

ruleTimeAfterNext :: Rule
ruleTimeAfterNext = Rule
  { name = "<time> after next"
  , pattern =
    [ regex "μεθεπ[οό]μ[εέ]ν(ο[ιυ]?ς?|ης?|ων)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ predNth 2 True td
      _ -> Nothing
  }

ruleThisDayofWeek :: Rule
ruleThisDayofWeek = Rule
  { name = "this <day-of-week>"
  , pattern =
    [ regex "αυτή[νς]? τη[νς]?"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth 0 True td
      _ -> Nothing
  }

ruleTheDayOf :: Rule
ruleTheDayOf = Rule
  { name = "on the day of <day>"
  , pattern =
    [ regex "ανήμερα"
    , Predicate $ isGrainOfTime TG.Day
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth 0 True td
      _ -> Nothing
  }

ruleThisDayofWeek2 :: Rule
ruleThisDayofWeek2 = Rule
  { name = "(this) coming <day-of-week>"
  , pattern =
    [ regex "ερχ[οό]μ[εέ]νη[ςν]?"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth 0 True td
      _ -> Nothing
  }

ruleBetweenTimeofdayAndTimeofdayInterval :: Rule
ruleBetweenTimeofdayAndTimeofdayInterval = Rule
  { name = "between <time-of-day> and <time-of-day> (interval)"
  , pattern =
    [ regex "μεταξύ|ανάμεσα"
    , Predicate isATimeOfDay
    , regex "(και(\\s+τ(ου|ης))?|\\-)"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleNextCycle :: Rule
ruleNextCycle = Rule
  { name = "next <cycle>"
  , pattern =
    [ regex "ε(π|ρχ)[οό]μ[εέ]ν(ο[ιυ]?ς?|ης?|ες|ων)"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) -> tt $ cycleNth grain 1
      _ -> Nothing
  }

ruleTimeofdayApproximately :: Rule
ruleTimeofdayApproximately = Rule
  { name = "<time-of-day> approximately"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "περίπου|και κάτι"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleInDurationFromTime :: Rule
ruleInDurationFromTime = Rule
  { name = "in <duration> from <time>"
  , pattern =
    [ regex "σε"
    , dimension Duration
    , regex "(ξεκινώντας\\s+)?από"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_:Token Time td1:_) ->
        tt $ durationAfter dd td1
      _ -> Nothing
}

ruleDurationFromNow :: Rule
ruleDurationFromNow = Rule
  { name = "<duration> from now"
  , pattern =
    [ dimension Duration
    , regex "από τώρα"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_) -> tt $ inDuration dd
      _ -> Nothing
  }

ruleLunch :: Rule
ruleLunch = Rule
  { name = "lunch"
  , pattern =
    [ regex "μεσημεριαν(ό|ού)( γεύμα(τος)?)?"
    ]
  , prod = \_ ->
      let from = hour False 12
          to = hour False 14
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleLastCycle :: Rule
ruleLastCycle = Rule
  { name = "last <cycle>"
  , pattern =
    [ regex "τελευταί(ου?|ας?)"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) -> tt . cycleNth grain $ - 1
      _ -> Nothing
  }

ruleAfternoon :: Rule
ruleAfternoon = Rule
  { name = "afternoon"
  , pattern =
    [ regex "απ[οό]γε[υύ]μα(τος)?"
    ]
  , prod = \_ ->
      let from = hour False 12
          to = hour False 18
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleDayofmonthOrdinalNamedmonth :: Rule
ruleDayofmonthOrdinalNamedmonth = Rule
  { name = "<day-of-month> (ordinal or number) <named-month>"
  , pattern =
    [ Predicate isDOMValue
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (day:Token Time month:_) -> Token Time <$> intersectDOM month day
      _ -> Nothing
  }

ruleNamedmonthDayofmonth :: Rule
ruleNamedmonthDayofmonth = Rule
  { name = "<named-month> <day-of-month> (ordinal or number)"
  , pattern =
    [ Predicate isAMonth
    , Predicate isDOMValue
    ]
  , prod = \tokens -> case tokens of
      (Token Time month:day:_) -> Token Time <$> intersectDOM month day
      _ -> Nothing
  }


ruleInduringThePartofday :: Rule
ruleInduringThePartofday = Rule
  { name = "in|during the <part-of-day>"
  , pattern =
    [ regex "κατά τη διάρκεια του|(μέσα )?σ?το"
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleHourofdayIntegerAsRelativeMinutes :: Rule
ruleHourofdayIntegerAsRelativeMinutes = Rule
  { name = "<hour-of-day> <integer> (as relative minutes)"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isAnHourOfDay]
    , regex "και"
    , Predicate $ isIntegerBetween 1 59
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData
       {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:_:
       token:
       _) -> do
        n <- getIntValue token
        tt $ hourMinute is12H hours n
      _ -> Nothing
  }

ruleNthTimeAfterTime :: Rule
ruleNthTimeAfterTime = Rule
  { name = "nth <time> after <time>"
  , pattern =
    [ dimension Ordinal
    , Predicate $ or . sequence
        [ isADayOfWeek
        , isAMonth
        ]
    , regex "μετά"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal OrdinalData{TOrdinal.value = v}:
       Token Time td1:
       _:
       Token Time td2:
       _) -> tt $ predNthAfter (v - 1) td1 td2
      _ -> Nothing
  }

ruleMmdd :: Rule
ruleMmdd = Rule
  { name = "dd/mm"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])\\s?[/.-]\\s?(1[0-2]|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        d <- parseInt m1
        m <- parseInt m2
        tt $ monthDay m d
      _ -> Nothing
  }

ruleAfterDuration :: Rule
ruleAfterDuration = Rule
  { name = "after <duration>"
  , pattern =
    [ regex "μετά\\s+από"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) ->
        tt . withDirection TTime.After $ inDuration dd
      _ -> Nothing
  }

ruleTimeofdayLatent :: Rule
ruleTimeofdayLatent = Rule
  { name = "time-of-day (latent)"
  , pattern =
    [ Predicate $ isIntegerBetween 0 23
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ hour (n < 12) n
      _ -> Nothing
  }

ruleExactlyTimeofday :: Rule
ruleExactlyTimeofday = Rule
  { name = "exactly <time-of-day>"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "ακριβώς"
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleBetweenDatetimeAndDatetimeInterval :: Rule
ruleBetweenDatetimeAndDatetimeInterval = Rule
  { name = "between <datetime> and <datetime> (interval)"
  , pattern =
    [ regex "μεταξύ|ανάμεσα"
    , Predicate isNotLatent
    , regex "(και|\\-)"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleDurationHenceAgo :: Rule
ruleDurationHenceAgo = Rule
  { name = "<duration> hence|ago"
  , pattern =
    [ dimension Duration
    , regex "(πριν|μετά)"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
        "πριν" -> tt $ durationAgo dd
        _ -> tt $ inDuration dd
      _ -> Nothing
  }

ruleHenceAgoDuration :: Rule
ruleHenceAgoDuration = Rule
  { name = "before <duration>"
  , pattern =
    [ regex "(πριν(\\s+από)?|εδώ και)"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       Token Duration dd:
       _) -> case Text.toLower match of
        "εδώ και" -> tt $ inDuration dd
        _ -> tt $ durationAgo dd
      _ -> Nothing
  }

ruleIntervalUntilTOD :: Rule
ruleIntervalUntilTOD = Rule
  { name = "until <time-of-day>"
  , pattern =
    [ regex "πριν"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ withDirection TTime.Before td
      _ -> Nothing
  }

ruleByTheEndOfTime :: Rule
ruleByTheEndOfTime = Rule
  { name = "by the end of <time>"
  , pattern =
    [ regex "μέχρι το (τέλος|πέρας)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> interval TTime.Closed td now
      _ -> Nothing
  }

ruleAfterWork :: Rule
ruleAfterWork = Rule
  { name = "after work"
  , pattern =
    [ regex "μετά τη δουλειά"
    ]
  , prod = \_ -> do
      td2 <- interval TTime.Open (hour False 17) (hour False 21)
      Token Time . partOfDay <$> intersect today td2
  }

ruleLastNCycle :: Rule
ruleLastNCycle = Rule
  { name = "last n <cycle>"
  , pattern =
    [ regex "(τελευταί|περασμέν)(ο[ιυ]?ς?|[εα]ς?|ων)"
    , Predicate $ isIntegerBetween 1 9999
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token TimeGrain grain:_) -> do
        n <- getIntValue token
        tt $ cycleN True grain (- n)
      _ -> Nothing
  }

ruleWithinDuration :: Rule
ruleWithinDuration = Rule
  { name = "within <duration>"
  , pattern =
    [ regex "μέσα(\\s+σε)?"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) -> Token Time <$>
        interval TTime.Open now (inDuration dd)
      _ -> Nothing
  }

ruleMidnight :: Rule
ruleMidnight = Rule
  { name = "midnight"
  , pattern =
      [ regex "μεσάνυχτα"
      ]
  , prod = const $ tt $ hour False 0
  }

ruleNoon :: Rule
ruleNoon = Rule
  { name = "noon"
  , pattern =
      [ regex "μεσημ[εέ]ρι(ού)?"
      ]
  , prod = const $ tt $ hour False 12
  }

ruleDayofmonthNonOrdinalNamedmonth :: Rule
ruleDayofmonthNonOrdinalNamedmonth = Rule
  { name = "<day-of-month> (non-ordinal) <named-month>"
  , pattern =
    [ Predicate isDOMInteger
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:token:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

rulePrecisionTimeofday :: Rule
rulePrecisionTimeofday = Rule
  { name = "about <time-of-day>"
  , pattern =
    [ regex "περίπου|ακριβώς"
    , Predicate $ isGrainFinerThan TG.Year
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleIntervalBy :: Rule
ruleIntervalBy = Rule
  { name = "until <time>"
  , pattern =
    [ regex "μέχρι"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> interval TTime.Open now td
      _ -> Nothing
  }

ruleIntervalByTheEndOf :: Rule
ruleIntervalByTheEndOf = Rule
  { name = "by the end of <time>"
  , pattern =
    [ regex "μέχρι το τέλος"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> interval TTime.Closed now td
      _ -> Nothing
  }

ruleUntilTimeofdayPostfix :: Rule
ruleUntilTimeofdayPostfix = Rule
  { name = "<time-of-day> until"
  , pattern =
    [ dimension Time
    , regex "το αργότερο"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ withDirection TTime.Before td
      _ -> Nothing
  }

ruleAtTimeofday :: Rule
ruleAtTimeofday = Rule
  { name = "at <time-of-day>"
  , pattern =
    [ regex "στις|@"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleNthTimeOfTime :: Rule
ruleNthTimeOfTime = Rule
  { name = "nth <time> of <time>"
  , pattern =
    [ dimension Ordinal
    , Predicate isNotLatent
    , regex "τ(ου|ης|ων)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal OrdinalData{TOrdinal.value = v}:
       Token Time td1:
       _:
       Token Time td2:
       _) -> Token Time . predNth (v - 1) False <$> intersect td2 td1
      _ -> Nothing
  }

ruleTimePartofday :: Rule
ruleTimePartofday = Rule
  { name = "<time> <part-of-day>"
  , pattern =
    [ dimension Time
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleWeekend :: Rule
ruleWeekend = Rule
  { name = "week-end"
  , pattern =
    [ regex "σαββατοκύριακ(ου?|α|ων)|σκ"
    ]
  , prod = const . tt . mkOkForThisNext $ weekend
  }

ruleLastWeekendOfMonth :: Rule
ruleLastWeekendOfMonth = Rule
  { name = "last weekend of <named-month>"
  , pattern =
    [ regex "τελευταίου?\\s+(σαββατοκύριακου?|σκ)\\s+"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td2:_) -> tt $ predLastOf weekend td2
      _ -> Nothing
  }

ruleNextDayofweek :: Rule
ruleNextDayofweek = Rule
  { name = "next <day-of-week>"
  , pattern =
    [ regex "επ[οό]μ[εέ]ν(ο[ιυ]?ς?|ης?|ες|ων)"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth 1 False td
      _ -> Nothing
  }

ruleNextTime :: Rule
ruleNextTime = Rule
  { name = "next <time>"
  , pattern =
    [ regex "επ[οό]μ[εέ]ν(ο[ιυ]?ς?|ης?|ες|ων)"
    , Predicate isOkWithThisNext
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth 1 False td
      _ -> Nothing
  }


ruleOrdinalQuarterYear :: Rule
ruleOrdinalQuarterYear = Rule
  { name = "<ordinal> quarter <year>"
  , pattern =
    [ dimension Ordinal
    , Predicate $ isGrain TG.Quarter
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal od:_:Token Time td:_) ->
        tt $ cycleNthAfter False TG.Quarter (TOrdinal.value od - 1) td
      _ -> Nothing
  }

ruleYyyymmdd :: Rule
ruleYyyymmdd = Rule
  { name = "yyyy-mm-dd"
  , pattern =
    [ regex "(\\d{2,4})[.-](1[0-2]|0?[1-9])[.-](3[01]|[12]\\d|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:m3:_)):_) -> do
        y <- parseInt m1
        m <- parseInt m2
        d <- parseInt m3
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

ruleOrdinalCycleAfterTime :: Rule
ruleOrdinalCycleAfterTime = Rule
  { name = "<ordinal> <cycle> after <time>"
  , pattern =
    [ dimension Ordinal
    , dimension TimeGrain
    , regex "μετά"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Ordinal od:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter True grain (TOrdinal.value od - 1) td
      _ -> Nothing
  }

ruleIntersect :: Rule
ruleIntersect = Rule
  { name = "intersect"
  , pattern =
    [ Predicate isNotLatent
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleIntersectBy :: Rule
ruleIntersectBy = Rule
  { name = "intersect by ','"
  , pattern =
    [ Predicate isNotLatent
    , regex ","
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleIntersectByOfFromS :: Rule
ruleIntersectByOfFromS = Rule
  { name = "intersect by 'of', 'from', 's"
  , pattern =
    [ Predicate isNotLatent
    , regex "τ(ου|ης)"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleNextNCycle :: Rule
ruleNextNCycle = Rule
  { name = "next n <cycle>"
  , pattern =
    [ regex "επ[οό]μ[εέ]ν(ο[ιυ]?ς?|ης?|ες|ων|α)"
    , Predicate $ isIntegerBetween 1 9999
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token TimeGrain grain:_) -> do
        v <- getIntValue token
        tt $ cycleN True grain v
      _ -> Nothing
  }

ruleADuration :: Rule
ruleADuration = Rule
  { name = "a <duration>"
  , pattern =
    [ regex "[εέ]ν(ας?|ός|ν)|μίας?"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) -> tt $ inDuration dd
      _ -> Nothing
  }

ruleMorning :: Rule
ruleMorning = Rule
  { name = "morning"
  , pattern =
    [ regex "(το\\s+)?πρωί"
    ]
  , prod = \_ ->
      let from = hour False 4
          to = hour False 12
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleEarlyMorning :: Rule
ruleEarlyMorning = Rule
  { name = "early morning"
  , pattern =
    [ regex "νωρίς\\s+(το\\s+)πρωί"
    ]
  , prod = \_ -> Token Time . partOfDay . mkLatent <$>
      interval TTime.Open (hour False 4) (hour False 9)
  }


ruleThisPartofday :: Rule
ruleThisPartofday = Rule
  { name = "this <part-of-day>"
  , pattern =
    [ regex "αυτ([οό]ύ?|ή[νς]?) τ(ον?|η[νς]?)"
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time . partOfDay <$> intersect today td
      _ -> Nothing
  }

ruleThisCycle :: Rule
ruleThisCycle = Rule
  { name = "this <cycle>"
  , pattern =
    [ regex "αυτ([οό][νύ]?|ή[νς]?)\\s+τ(ον?|η[νς]?)"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) -> tt $ cycleNth grain 0
      _ -> Nothing
  }

ruleDayofmonthNonOrdinalOfNamedmonth :: Rule
ruleDayofmonthNonOrdinalOfNamedmonth = Rule
  { name = "<day-of-month> (ordinal or number) of <named-month>"
  , pattern =
    [ Predicate isDOMValue
    , regex "του"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (day:_:Token Time td:_) -> Token Time <$> intersectDOM td day
      _ -> Nothing
  }

ruleYearLatent :: Rule
ruleYearLatent = Rule
  { name = "year (latent)"
  , pattern =
    [ Predicate $
        or . sequence [isIntegerBetween (- 10000) 0, isIntegerBetween 25 999]
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        y <- getIntValue token
        tt . mkLatent $ year y
      _ -> Nothing
  }

ruleAfterTimeofday :: Rule
ruleAfterTimeofday = Rule
  { name = "after <time-of-day>"
  , pattern =
    [ regex "μετά"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ withDirection TTime.After td
      _ -> Nothing
  }

ruleCycleThis :: Rule
ruleCycleThis = Rule
  { name = "this <cycle>"
  , pattern =
    [ regex $ "αυτ([οό][υύ]?|[ηή]|[εέ]|ώ|ά)ν?ς?|"
           ++ "τρ[εέ]χ(ουσ)?(α|ε|ο|ω|ώ)ν?ς?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth 0 False td
      _ -> Nothing
  }

cyclesMap :: HashMap Text Int
cyclesMap = HashMap.fromList
  [ ("τρεχ"      , 0)
  , ("τρέχ"      , 0)
  , ("ερχόμεν"   , 0)
  , ("ερχομέν"   , 0)
  , ("επόμεν"    , 1)
  , ("επομέν"    , 1)
  , ("περασμέν"  , -1)
  , ("προηγούμεν", -1)
  , ("προηγουμέν", -1)
  ]

ruleCycleCurrentLastNext :: Rule
ruleCycleCurrentLastNext = Rule
  { name = "this|last|next <cycle>"
  , pattern =
    [ regex $ "(τρ[εέ]χ|επ[οό]μ[εέ]ν|ερχ[οό]μ[εέ]ν|περασμέν|προηγο[υύ]μ[εέ]ν)"
           ++ "(ουσ)?(α|ά|ο[υύ]?|η|ή|ε|έ|ω|ώ)ν?ς?"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):Token TimeGrain grain:_) ->
        HashMap.lookup (Text.toLower match) cyclesMap >>= tt . cycleNth grain
      _ -> Nothing
  }

ruleNight :: Rule
ruleNight = Rule
  { name = "night"
  , pattern =
    [ regex "νύχτα"
    ]
  , prod = const $
      let from = hour False 20
          to = hour False 8
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleDayofmonthOrdinal :: Rule
ruleDayofmonthOrdinal = Rule
  { name = "<day-of-month> (ordinal)"
  , pattern =
    [ Predicate isDOMOrdinal
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal OrdinalData{TOrdinal.value = v}:_) ->
        tt . mkLatent $ dayOfMonth v
      _ -> Nothing
  }

ruleTimeofdayAmpm :: Rule
ruleTimeofdayAmpm = Rule
  { name = "<time-of-day> am|pm"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "([πμ])\\.?(μ\\.?)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:Token RegexMatch (GroupMatch (ap:_)):_) ->
        tt $ timeOfDayAMPM (Text.toLower ap == "π") td
      _ -> Nothing
  }

ruleTimeofdayAmpmVerbose :: Rule
ruleTimeofdayAmpmVerbose = Rule
  { name = "<time-of-day> am|pm (verbose)"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "το\\s+(πρωί|απόγευμα)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:Token RegexMatch (GroupMatch (ap:_)):_) ->
        tt $ timeOfDayAMPM (Text.toLower ap == "πρωί") td
      _ -> Nothing
  }

ruleAfterNextTime :: Rule
ruleAfterNextTime = Rule
  { name = "after next <time>"
  , pattern =
    [ regex "μετά τ(ο|η)ν? επόμεν(ο|η)ν?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth 1 True td
      _ -> Nothing
  }

ruleHhmm :: Rule
ruleHhmm = Rule
  { name = "hh:mm"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3]))[:ω]([0-5]\\d)ω?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        h <- parseInt m1
        m <- parseInt m2
        tt $ hourMinute True h m
      _ -> Nothing
  }

ruleHHMMSS :: Rule
ruleHHMMSS = Rule
  { name = "hh:mm:ss"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3]))[:.]([0-5]\\d)[:.]([0-5]\\d)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (hh:mm:ss:_)):_) -> do
        h <- parseInt hh
        m <- parseInt mm
        s <- parseInt ss
        tt $ hourMinuteSecond True h m s
      _ -> Nothing
  }

-- We can't take generic TOD (e.g. "6:30am - 9pm").
-- Those are handled by other rules.
ruleIntervalTODAMPM :: Rule
ruleIntervalTODAMPM = Rule
 { name = "hh(:mm) - <time-of-day> am|pm"
 , pattern =
   [ regex "(?:από )?((?:[01]?\\d)|(?:2[0-3]))([:.]([0-5]\\d))?"
   , regex "\\-|:|μέχρι"
   , Predicate isATimeOfDay
   , regex "([πμ])(\\s|\\.)?(μ\\.?)?"
   ]
 , prod = \tokens -> case tokens of
     (Token RegexMatch (GroupMatch (hh:_:mm:_)):
      _:
      Token Time td2:
      Token RegexMatch (GroupMatch (ap:_)):
      _) -> do
       h <- parseInt hh
       let ampm = Text.toLower ap == "π"
           td1 = maybe (hour True h) (hourMinute True h) (parseInt mm)
       Token Time <$>
         interval TTime.Closed (timeOfDayAMPM ampm td1) (timeOfDayAMPM ampm td2)
     _ -> Nothing
 }

-- We can't take generic TOD (e.g. "6:30am - 9pm").
-- Those are handled by other rules.
ruleIntervalTODAMPMverbose :: Rule
ruleIntervalTODAMPMverbose = Rule
 { name = "hh(:mm) - <time-of-day> am|pm (verbose)"
 , pattern =
   [ regex "(?:από\\s+)?((?:[01]?\\d)|(?:2[0-3]))([:.]([0-5]\\d))?"
   , regex "\\-|:|μέχρι"
   , Predicate isATimeOfDay
   , regex "(?:το\\s+)?(πρωί|απόγευμα)"
   ]
 , prod = \tokens -> case tokens of
     (Token RegexMatch (GroupMatch (hh:_:mm:_)):
      _:
      Token Time td2:
      Token RegexMatch (GroupMatch (ap:_)):
      _) -> do
       h <- parseInt hh
       let ampm = Text.toLower ap == "πρωί"
           td1 = maybe (hour True h) (hourMinute True h) (parseInt mm)
       Token Time <$>
         interval TTime.Closed (timeOfDayAMPM ampm td1) (timeOfDayAMPM ampm td2)
     _ -> Nothing
 }

ruleTonight :: Rule
ruleTonight = Rule
  { name = "tonight"
  , pattern =
    [ regex "απόψε"
    ]
  , prod = \_ -> do
      td2 <- interval TTime.Open (hour False 18) (hour False 0)
      Token Time . partOfDay <$> intersect today td2
  }

ruleTomorrowNight :: Rule
ruleTomorrowNight = Rule
  { name = "tomorrownight"
  , pattern =
    [ regex "αύριο\\s+(το\\s+)?βράδυ"
    ]
  , prod = \_ -> do
      let td1 = cycleNth TG.Day 1
      td2 <- interval TTime.Open (hour False 18) (hour False 0)
      Token Time . partOfDay <$> intersect td1 td2
  }

ruleLastNight :: Rule
ruleLastNight = Rule
  { name = "lastnight"
  , pattern =
    [ regex "ε?χ[θτ][εέ]ς\\s+(το\\s+)?βράδυ"
    ]
  , prod = \_ -> do
      let td1 = cycleNth TG.Day $ - 1
      td2 <- interval TTime.Open (hour False 18) (hour False 0)
      Token Time . partOfDay <$> intersect td1 td2
  }

ruleYear :: Rule
ruleYear = Rule
  { name = "year"
  , pattern =
    [ Predicate $ isIntegerBetween 1000 2100
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        y <- getIntValue token
        tt $ year y
      _ -> Nothing
  }

ruleThisYear :: Rule
ruleThisYear = Rule
  { name = "this year"
  , pattern =
    [ regex "ε?φ[εέ]τος?"
    ]
  , prod = const . tt $ cycleNth TG.Year 0
  }

ruleLastYear :: Rule
ruleLastYear = Rule
  { name = "Last year"
  , pattern =
    [ regex "π[εέ]ρ(υ?σιν|ασμέν)[οόηή]ς?\\s+(έτου?ς|χρ[οό]ν(ου|ιάς?))"
    ]
  , prod = const . tt . cycleNth TG.Year $ - 1
  }

ruleLastYearOneWord :: Rule
ruleLastYearOneWord = Rule
  { name = "Last year"
  , pattern =
    [ regex "π[εέ]ρυ?σι"
    ]
  , prod = const . tt . cycleNth TG.Year $ - 1
  }

ruleNextYear :: Rule
ruleNextYear = Rule
  { name = "Last year"
  , pattern =
    [ regex "του\\s*χρόνου"
    ]
  , prod = const . tt $ cycleNth TG.Year 1
  }

ruleHhmmMilitary :: Rule
ruleHhmmMilitary = Rule
  { name = "hhmm (military)"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3]))([0-5]\\d)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (h:m:_)):_) -> do
        hh <- parseInt h
        mm <- parseInt m
        tt . mkLatent $ hourMinute False hh mm
      _ -> Nothing
  }

ruleAbsorptionOfAfterNamedDay :: Rule
ruleAbsorptionOfAfterNamedDay = Rule
  { name = "absorption of , after named day"
  , pattern =
    [ Predicate isADayOfWeek
    , regex ","
    ]
  , prod = \tokens -> case tokens of
      (x:_) -> Just x
      _ -> Nothing
  }

ruleAbsorptionOfArticleBeforeTime :: Rule
ruleAbsorptionOfArticleBeforeTime = Rule
  { name = "absorption of article before time"
  , pattern =
    [ regex "σ?τ?(α|η|ι|ο[ιυ]?|ω)ν?ς?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleLastDayofweekOfTime :: Rule
ruleLastDayofweekOfTime = Rule
  { name = "last <day-of-week> of <time>"
  , pattern =
    [ regex "τελευταί(ου?|ας?)"
    , Predicate isADayOfWeek
    , regex "τ(ου|ης)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) -> tt $ predLastOf td1 td2
      _ -> Nothing
  }

ruleHhmmMilitaryAmpm :: Rule
ruleHhmmMilitaryAmpm = Rule
  { name = "hhmm (military) am|pm"
  , pattern =
    [ regex "((?:1[012]|0?\\d))([0-5]\\d)"
    , regex "([πμ])\\.?(μ\\.?)?"
    ]
  , prod = \tokens -> case tokens of
      ( Token RegexMatch (GroupMatch (hh:mm:_)):
        Token RegexMatch (GroupMatch (ap:_)):_) -> do
        h <- parseInt hh
        m <- parseInt mm
        tt $ timeOfDayAMPM (Text.toLower ap == "π") (hourMinute True h m)
      _ -> Nothing
  }

ruleTimeofdayTimeofdayInterval :: Rule
ruleTimeofdayTimeofdayInterval = Rule
  { name = "<time-of-day> - <time-of-day> (interval)"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isATimeOfDay]
    , regex "\\-|μέχρι"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleDurationAfterTime :: Rule
ruleDurationAfterTime = Rule
  { name = "<duration> after <time>"
  , pattern =
    [ dimension Duration
    , regex "μετά(\\s+από)?|από"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_:Token Time td:_) -> tt $ durationAfter dd td
      _ -> Nothing
  }

ruleOrdinalQuarter :: Rule
ruleOrdinalQuarter = Rule
  { name = "<ordinal> quarter"
  , pattern =
    [ dimension Ordinal
    , Predicate $ isGrain TG.Quarter
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal OrdinalData{TOrdinal.value = v}:_) -> tt .
        cycleNthAfter False TG.Quarter (v - 1) $ cycleNth TG.Year 0
      _ -> Nothing
  }

ruleDurationBeforeTime :: Rule
ruleDurationBeforeTime = Rule
  { name = "<duration> before <time>"
  , pattern =
    [ dimension Duration
    , regex "πριν"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_:Token Time td:_) -> tt $ durationBefore dd td
      _ -> Nothing
  }

rulePartofdayOfTime :: Rule
rulePartofdayOfTime = Rule
  { name = "<part-of-day> of <time>"
  , pattern =
    [ Predicate isAPartOfDay
    , regex "τ(ου|ης)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) -> Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleMmddyyyy :: Rule
ruleMmddyyyy = Rule
  { name = "dd/mm/yyyy"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])[/.-](1[0-2]|0?[1-9])[/.-](\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:m3:_)):_) -> do
        y <- parseInt m3
        m <- parseInt m2
        d <- parseInt m1
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

ruleTimeofdayOclock :: Rule
ruleTimeofdayOclock = Rule
  { name = "<time-of-day>  o'clock"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "η ώρα"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleDayofmonthordinalNamedmonthYear :: Rule
ruleDayofmonthordinalNamedmonthYear = Rule
  { name = "<day-of-month> ( ordinal) <named-month> year"
  , pattern =
    [ Predicate isDOMOrdinal
    , Predicate isAMonth
    , regex "(\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (dd:Token Time mm:Token RegexMatch (GroupMatch (yy:_)):_) -> do
        y <- parseInt yy
        dom <- intersectDOM mm dd
        Token Time <$> intersect dom (year y)
      _ -> Nothing
  }

ruleIntervalForDurationFrom :: Rule
ruleIntervalForDurationFrom = Rule
  { name = "for <duration> from <time>"
  , pattern =
    [ regex "για"
    , dimension Duration
    , regex "(ξεκινώντας\\s+)?(μετά(\\s+από)?|από)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_:Token Time td1:_) ->
        Token Time <$> interval TTime.Open td1 (durationAfter dd td1)
      _ -> Nothing
}

ruleTimezone :: Rule
ruleTimezone = Rule
  { name = "<time> timezone"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isATimeOfDay]
    , regex "\\b(YEKT|YEKST|YAKT|YAKST|WITA|WIT|WIB|WGT|WGST|WFT|WET|WEST|WAT|WAST|VUT|VLAT|VLAST|VET|UZT|UYT|UYST|UTC|ULAT|TVT|TMT|TLT|TKT|TJT|TFT|TAHT|SST|SRT|SGT|SCT|SBT|SAST|SAMT|RET|PYT|PYST|PWT|PST|PONT|PMST|PMDT|PKT|PHT|PHOT|PGT|PETT|PETST|PET|PDT|OMST|OMSST|NZST|NZDT|NUT|NST|NPT|NOVT|NOVST|NFT|NDT|NCT|MYT|MVT|MUT|MST|MSK|MSD|MMT|MHT|MDT|MAWT|MART|MAGT|MAGST|LINT|LHST|LHDT|KUYT|KST|KRAT|KRAST|KGT|JST|IST|IRST|IRKT|IRKST|IRDT|IOT|IDT|ICT|HOVT|HKT|GYT|GST|GMT|GILT|GFT|GET|GAMT|GALT|FNT|FKT|FKST|FJT|FJST|EST|EGT|EGST|EET|EEST|EDT|ECT|EAT|EAST|EASST|DAVT|ChST|CXT|CVT|CST|COT|CLT|CLST|CKT|CHAST|CHADT|CET|CEST|CDT|CCT|CAT|CAST|BTT|BST|BRT|BRST|BOT|BNT|AZT|AZST|AZOT|AZOST|AWST|AWDT|AST|ART|AQTT|ANAT|ANAST|AMT|AMST|ALMT|AKST|AKDT|AFT|AEST|AEDT|ADT|ACST|ACDT)\\b"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:
       Token RegexMatch (GroupMatch (tz:_)):
       _) -> Token Time <$> inTimezone (Text.toUpper tz) td
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleADuration
  , ruleAbsorptionOfAfterNamedDay
  , ruleAbsorptionOfArticleBeforeTime
  , ruleAfterDuration
  , ruleAfterNextTime
  , ruleAfterTimeofday
  , ruleAfterWork
  , ruleAfternoon
  , ruleAtTimeofday
  , ruleBetweenDatetimeAndDatetimeInterval
  , ruleBetweenTimeofdayAndTimeofdayInterval
  , ruleByTheEndOfTime
  , ruleCycleCurrentLastNext
  , ruleCycleThis
  , ruleDatetimeDatetimeInterval
  , ruleDayofmonthNonOrdinalNamedmonth
  , ruleDayofmonthNonOrdinalOfNamedmonth
  , ruleDayofmonthOrdinal
  , ruleDayofmonthOrdinalNamedmonth
  , ruleDayofmonthordinalNamedmonthYear
  , ruleDurationAfterTime
  , ruleDurationBeforeTime
  , ruleDurationFromNow
  , ruleDurationHenceAgo
  , ruleEarlyMorning
  , ruleEvening
  , ruleExactlyTimeofday
  , ruleFromDatetimeDatetimeInterval
  , ruleHHMMSS
  , ruleHalfAfterIntegerHourofday
  , ruleHalfAfterIntegerHourofday2
  , ruleHenceAgoDuration
  , ruleHhmm
  , ruleHhmmMilitary
  , ruleHhmmMilitaryAmpm
  , ruleHourofdayIntegerAsRelativeMinutes
  , ruleInDuration
  , ruleInDurationFromTime
  , ruleInduringThePartofday
  , ruleIntersect
  , ruleIntersectBy
  , ruleIntersectByOfFromS
  , ruleIntervalBy
  , ruleIntervalByTheEndOf
  , ruleIntervalForDurationFrom
  , ruleIntervalTODAMPM
  , ruleIntervalTODAMPMverbose
  , ruleIntervalTODDash
  , ruleIntervalUntilTOD
  , ruleLastCycle
  , ruleLastCycleOfTime
  , ruleLastCycleOfTimeS
  , ruleLastDayofweekOfTime
  , ruleLastNCycle
  , ruleLastNight
  , ruleLastTime
  , ruleLastWeekendOfMonth
  , ruleLastYear
  , ruleLastYearOneWord
  , ruleLunch
  , ruleMidnight
  , ruleMmdd
  , ruleMmddyyyy
  , ruleMonthDdddInterval
  , ruleMorning
  , ruleNamedmonthDayofmonth
  , ruleNextCycle
  , ruleNextDayofweek
  , ruleNextNCycle
  , ruleNextTime
  , ruleNextYear
  , ruleNight
  , ruleNoon
  , ruleNthTimeAfterTime
  , ruleNthTimeOfTime
  , ruleOrdinalCycleAfterTime
  , ruleOrdinalCycleOfTime
  , ruleOrdinalQuarter
  , ruleOrdinalQuarterYear
  , rulePartOfMonth
  , rulePartofdayOfTime
  , rulePrecisionTimeofday
  , ruleQuarterBeforeOrAfterIntegerHourofday
  , ruleRelativeIntegerToOrAfterIntegerPartOfDay
  , ruleTheCycleAfterTime
  , ruleTheCycleBeforeTime
  , ruleTheCycleOfTime
  , ruleTheDayOf
  , ruleTheDayofmonthNonOrdinal
  , ruleThisCycle
  , ruleThisDayofWeek
  , ruleThisDayofWeek2
  , ruleThisPartofday
  , ruleThisYear
  , ruleTimeAfterNext
  , ruleTimePartofday
  , ruleTimeofdayAmpm
  , ruleTimeofdayAmpmVerbose
  , ruleTimeofdayApproximately
  , ruleTimeofdayLatent
  , ruleTimeofdayOclock
  , ruleTimeofdayTimeofdayInterval
  , ruleTimezone
  , ruleTomorrowNight
  , ruleTonight
  , ruleUntilTimeofdayPostfix
  , ruleWeekend
  , ruleWithinDuration
  , ruleYear
  , ruleYearLatent
  , ruleYearLatent2
  , ruleYyyymmdd
  ]
  ++ ruleDaysOfWeek
  ++ ruleHolidays
  ++ ruleInstants
  ++ ruleMonths
  ++ ruleSeasons
