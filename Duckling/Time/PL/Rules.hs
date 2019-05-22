-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.PL.Rules
  ( rules ) where

import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Types (OrdinalData(..))
import Duckling.Regex.Types
import Duckling.Time.Helpers
import Duckling.Time.Types (TimeData (..))
import Duckling.Types
import qualified Duckling.Ordinal.Types as TOrdinal
import qualified Duckling.Time.Types as TTime
import qualified Duckling.TimeGrain.Types as TG

ruleOrdinalCycleTime :: Rule
ruleOrdinalCycleTime = Rule
  { name = "<ordinal> <cycle> <time>"
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

ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = mkRuleDaysOfWeek
  [ ( "Monday" , "poniedzia(l|ł)(ek|ku|kowi|kiem|kowy)|pon\\.?" )
  , ( "Tuesday" , "wtorek|wtorku|wtorkowi|wtorkiem|wtr?\\.?" )
  , ( "Wednesday" , "(Ś|ś|s)rod(a|ą|y|e|ę|zie|owy|o)|(s|ś|Ś)ro?\\.?" )
  , ( "Thursday" , "czwartek|czwartku|czwartkowi|czwartkiem|czwr?\\.?" )
  , ( "Friday" , "piątek|piatek|piątku|piatku|piątkowi|piatkowi|piątkiem|piatkiem|pi(ą|a)tkowy|pia\\.?" )
  , ( "Saturday" , "sobota|soboty|sobocie|sobotę|sobote|sobotą|sobota|sobocie|soboto|sob\\.?" )
  , ( "Sunday" , "niedziel(a|i|ę|e|ą|o)|n(ie)?dz?\\.?" )
  ]

ruleMonths :: [Rule]
ruleMonths = mkRuleMonths
  [ ( "January" , "stycz(eń|en|nia|niowi|niem|niu)|sty(cz)?\\.?" )
  , ( "February" , "lu(ty|tego|temu|t?\\.?)" )
  , ( "March" , "mar(zec|ca|cowi|cem|cu|z?\\.?)" )
  , ( "April" , "kwie(cień|cien|tnia|tniowi|tniem|tniu|t?\\.?)" )
  , ( "May" , "maj(a|owi|em|u)?" )
  , ( "June" , "czerw(iec|ca|cowi|cem|cu)|czer?\\.?" )
  , ( "July" , "lip(iec|ca|cowi|cem|cu)|lip\\.?" )
  , ( "August" , "sierp(ie(n|ń)|ni(a|owi|em|u))|sierp\\.?|sier\\.?|sie\\.?" )
  , ( "September" , "wrze(s|ś)(ie(ń|n)|ni(a|owi|em|u))|wrz\\.?|wrze\\.?" )
  , ( "October" , "pa(z|ź)dziernik(a|owi|iem|u)?|paź\\.?|paz\\.?" )
  , ( "November" , "listopad(a|owi|em|zie)?|lis\\.?|list\\.?" )
  , ( "December" , "grudz(ień|ien)|grudn(ia|iowi|iem|niu)|gru\\.?|grud\\.?" )
  ]

ruleRelativeMinutesTotillbeforeIntegerHourofday :: Rule
ruleRelativeMinutesTotillbeforeIntegerHourofday = Rule
  { name = "relative minutes to|till|before <integer> (hour-of-day)"
  , pattern =
    [ Predicate $ isIntegerBetween 1 59
    , regex "do|przed"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token Time td:_) -> do
        n <- getIntValue token
        Token Time <$> minutesBefore n td
      _ -> Nothing
  }

ruleRelativeMinutesAfterpastIntegerHourofday :: Rule
ruleRelativeMinutesAfterpastIntegerHourofday = Rule
  { name = "relative minutes after|past <integer> (hour-of-day)"
  , pattern =
    [ Predicate $ isIntegerBetween 1 59
    , regex "po"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (token:
       _:
       Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) _)}:
       _) -> do
        n <- getIntValue token
        tt $ hourMinute True hours n
      _ -> Nothing
  }

ruleHourofdayIntegerAsRelativeMinutes :: Rule
ruleHourofdayIntegerAsRelativeMinutes = Rule
  { name = "<hour-of-day> <integer> (as relative minutes)"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isAnHourOfDay]
    , Predicate $ isIntegerBetween 1 59
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) _)}:
       token:
       _) -> do
        n <- getIntValue token
        tt $ hourMinute True hours n
      _ -> Nothing
  }

ruleQuarterTotillbeforeIntegerHourofday :: Rule
ruleQuarterTotillbeforeIntegerHourofday = Rule
  { name = "quarter to|till|before <integer> (hour-of-day)"
  , pattern =
    [ regex "kwadrans(ie|owi|em|a)? *(do|przed)"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> minutesBefore 15 td
      _ -> Nothing
  }

ruleQuarterAfterpastIntegerHourofday :: Rule
ruleQuarterAfterpastIntegerHourofday = Rule
  { name = "quarter after|past <integer> (hour-of-day)"
  , pattern =
    [ regex "kwadrans(ie|owi|em|a)? *po"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) _)}:
       _) -> tt $ hourMinute True hours 15
      _ -> Nothing
  }

ruleHourofdayQuarter :: Rule
ruleHourofdayQuarter = Rule
  { name = "<hour-of-day> quarter (as relative minutes)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "kwadrans(ie|owi|em|a)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) _)}:
       _) -> tt $ hourMinute True hours 15
      _ -> Nothing
  }

ruleHalfTotillbeforeIntegerHourofday :: Rule
ruleHalfTotillbeforeIntegerHourofday = Rule
  { name = "half to|till|before <integer> (hour-of-day)"
  , pattern =
    [ regex "p(o|ó)(l|ł) *(do|przed)"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> minutesBefore 30 td
      _ -> Nothing
  }

ruleHalfAfterpastIntegerHourofday :: Rule
ruleHalfAfterpastIntegerHourofday = Rule
  { name = "half after|past <integer> (hour-of-day)"
  , pattern =
    [ regex "p(o|ó)(l|ł) *po"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) _)}:
       _) -> tt $ hourMinute True hours 30
      _ -> Nothing
  }

ruleHourofdayHalf :: Rule
ruleHourofdayHalf = Rule
  { name = "<hour-of-day> half (as relative minutes)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "p(o|ó)(l|ł)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) _)}:
       _) -> tt $ hourMinute True hours 30
      _ -> Nothing
  }

ruleValentinesDay :: Rule
ruleValentinesDay = Rule
  { name = "valentine's day"
  , pattern =
    [ regex "walentynki"
    ]
  , prod = \_ -> tt $ monthDay 2 14
  }

ruleLastTime :: Rule
ruleLastTime = Rule
  { name = "last <time>"
  , pattern =
    [ regex "ostatni(ego|ch|emu|mi|m|(a|ą)|ej)?|(po ?)?przedni(ego|ch|emu|e|mi|m|a)?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ predNth (-1) False td
      _ -> Nothing
  }

ruleDatetimeDatetimeInterval :: Rule
ruleDatetimeDatetimeInterval = Rule
  { name = "<datetime> - <datetime> (interval)"
  , pattern =
    [ Predicate isNotLatent
    , regex "\\-|(p|d)o|a(ż|z) (p|d)o"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleCycleAfterTime :: Rule
ruleCycleAfterTime = Rule
  { name = "<cycle> after <time>"
  , pattern =
    [ dimension TimeGrain
    , regex "po"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter False grain 1 td
      _ -> Nothing
  }

ruleInDuration :: Rule
ruleInDuration = Rule
  { name = "in <duration>"
  , pattern =
    [ regex "(w( ?(prze)?ciągu)?|za) ?(jeszcze)?|przez"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) ->
        tt $ inDuration dd
      _ -> Nothing
  }

ruleNow :: Rule
ruleNow = Rule
  { name = "now"
  , pattern =
    [ regex "(w)? ?(tym|tej)? ?(teraz|momencie|chwili|momeńcie)"
    ]
  , prod = \_ -> tt now
  }

ruleLastCycleOfTime :: Rule
ruleLastCycleOfTime = Rule
  { name = "last <cycle> of <time>"
  , pattern =
    [ regex "ostatni(ego|ch|emu|mi|m|(a|ą)|ej)?"
    , dimension TimeGrain
    , regex "w(e)?|z(e)?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleLastOf grain td
      _ -> Nothing
  }

ruleFromDatetimeDatetimeInterval :: Rule
ruleFromDatetimeDatetimeInterval = Rule
  { name = "from <datetime> - <datetime> (interval)"
  , pattern =
    [ regex "od|p(o|ó)(z|ź)niej ni(z|ż)"
    , dimension Time
    , regex "\\-|do|po|aż do|az do|aż po|az po|ale przed"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleOrdinalAsHour :: Rule
ruleOrdinalAsHour = Rule
  { name = "<ordinal> (as hour)"
  , pattern =
    [ Predicate $ isOrdinalBetween 1 24
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal OrdinalData{TOrdinal.value = v}:_) ->
        tt . mkLatent $ hour True v
      _ -> Nothing
  }

ruleMonthDdddInterval :: Rule
ruleMonthDdddInterval = Rule
  { name = "<month> dd-dd (interval)"
  , pattern =
    [ Predicate isAMonth
    , regex "(3[01]|[12]\\d|0?[1-9])"
    , regex "\\-|do|po|aż do|az do|aż po|az po"
    , regex "(3[01]|[12]\\d|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      ( Token Time td
       :Token RegexMatch (GroupMatch (d1:_))
       :_
       :Token RegexMatch (GroupMatch (d2:_))
       :_) -> do
        dd1 <- parseInt d1
        dd2 <- parseInt d2
        dom1 <- intersect (dayOfMonth dd1) td
        dom2 <- intersect (dayOfMonth dd2) td
        Token Time <$> interval TTime.Closed dom1 dom2
      _ -> Nothing
  }

ruleSeason4 :: Rule
ruleSeason4 = Rule
  { name = "season"
  , pattern =
    [ regex "wiosna|wiosny|wiośnie|wiosnie|wiosnę|wiosne|wiosną|wiosna|wiośnie|wiosnie|wiosno"
    ]
  , prod = \_ ->
      let from = monthDay 3 20
          to = monthDay 6 21
      in Token Time <$> interval TTime.Open from to
  }

ruleYearLatent2 :: Rule
ruleYearLatent2 = Rule
  { name = "year (latent)"
  , pattern =
    [ Predicate $ isIntegerBetween 2101 10000
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ year n
      _ -> Nothing
  }

ruleTimeAfterNext :: Rule
ruleTimeAfterNext = Rule
  { name = "<time> after next"
  , pattern =
    [ dimension Time
    , regex "po kolejnym|po nast(e|ę)pn(ym|y|ego|emu|(a|ą)|ej|e)|po przysz(l|ł)(ym|y|ego|emu|(a|ą)|ej)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ predNth 1 True td
      _ -> Nothing
  }

ruleTheIdesOfNamedmonth :: Rule
ruleTheIdesOfNamedmonth = Rule
  { name = "the ides of <named-month>"
  , pattern =
    [ regex "the ides? of"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td@TimeData {TTime.form = Just (TTime.Month m)}:_) ->
        Token Time <$>
          intersect (dayOfMonth $ if elem m [3, 5, 7, 10] then 15 else 13) td
      _ -> Nothing
  }

ruleNoon :: Rule
ruleNoon = Rule
  { name = "noon"
  , pattern =
    [ regex "po(l|ł)udni(em|e|a|u)"
    ]
  , prod = \_ -> tt $ hour False 12
  }

ruleToday :: Rule
ruleToday = Rule
  { name = "today"
  , pattern =
    [ regex "dzisiejszy|dzisiaj|dzi[śs]|w ten dzień|w ten dzien|w obecny dzień|w obecny dzien|obecnego dnia"
    ]
  , prod = \_ -> tt today
  }

ruleThisnextDayofweek :: Rule
ruleThisnextDayofweek = Rule
  { name = "this|next <day-of-week>"
  , pattern =
    [ regex "kolejn(ym|y|ego|emu|e)|nast(e|ę)pn(ym|y|ego|emu|e|(a|ą)|ej|e)"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ predNth 0 True td
      _ -> Nothing
  }

ruleBetweenTimeofdayAndTimeofdayInterval :: Rule
ruleBetweenTimeofdayAndTimeofdayInterval = Rule
  { name = "between <time-of-day> and <time-of-day> (interval)"
  , pattern =
    [ regex "(po|po )?miedzy|między"
    , Predicate isATimeOfDay
    , regex "a|i"
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
    [ regex "kolejn(ym|y|ego|emu|(a|ą)|ej|e)|nast(e|ę)pn(ym|y|ego|emu|(a|ą)|ej|e)|przysz(l|ł)(ego|emu|ym|(a|ą)|ej|ych|i|ymi|y|e)|za"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) ->
        tt $ cycleNth grain 1
      _ -> Nothing
  }

ruleTheCycleOfTime :: Rule
ruleTheCycleOfTime = Rule
  { name = "the <cycle> of <time>"
  , pattern =
    [ regex "the"
    , dimension TimeGrain
    , regex "of"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter True grain 0 td
      _ -> Nothing
  }

ruleTimeofdayApproximately :: Rule
ruleTimeofdayApproximately = Rule
  { name = "<time-of-day> approximately"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "o?ko(l|ł)o|mniej wi(e|ę)cej"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

rulePolishIndependenceDay :: Rule
rulePolishIndependenceDay = Rule
  { name = "Polish independence day"
  , pattern =
    [ regex "(s|ś)wiet(a|o) niepodleg(l|ł)o(s|ś)ci|(ś|s)w\\.? niepodleg(l|ł)o(s|ś)ci"
    ]
  , prod = \_ -> tt $ monthDay 11 11
  }

ruleOnDate :: Rule
ruleOnDate = Rule
  { name = "on <date>"
  , pattern =
    [ regex "we?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:x:_) -> Just x
      _ -> Nothing
  }

ruleLastDayofweekTime :: Rule
ruleLastDayofweekTime = Rule
  { name = "last <day-of-week> <time>"
  , pattern =
    [ regex "ostatni(ego|ch|emu|mi|m|(a|ą)|ej)?"
    , Predicate isADayOfWeek
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:Token Time td2:_) ->
        tt $ predLastOf td1 td2
      _ -> Nothing
  }

ruleDurationFromNow :: Rule
ruleDurationFromNow = Rule
  { name = "<duration> from now"
  , pattern =
    [ dimension Duration
    , regex "od (dzi(s|ś)|teraz)"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_) ->
        tt $ inDuration dd
      _ -> Nothing
  }

ruleLunch :: Rule
ruleLunch = Rule
  { name = "lunch"
  , pattern =
    [ regex "(na )?la?u?nc(z|h)|obiad"
    ]
  , prod = \_ ->
      let from = hour False 12
          to = hour False 14
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleAfternoon :: Rule
ruleAfternoon = Rule
  { name = "afternoon"
  , pattern =
    [ regex "po(l|ł)udni(em|e|a|u)"
    ]
  , prod = \_ ->
      let from = hour False 12
          to = hour False 19
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleEveningnight :: Rule
ruleEveningnight = Rule
  { name = "evening|night"
  , pattern =
    [ regex "wiecz[oó]r(em|owi|ze|a|u)?|nocą?"
    ]
  , prod = \_ ->
      let from = hour False 18
          to = hour False 0
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleLastCycle :: Rule
ruleLastCycle = Rule
  { name = "last <cycle>"
  , pattern =
    [ regex "ostatni(ego|ch|emu|mi|m|(a|ą)|ej|e)?|(po ?)?przedni(ego|ch|emu|mi|m|e|(a|ą)|ej)?"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) ->
        tt . cycleNth grain $ - 1
      _ -> Nothing
  }

ruleHourofdayHourofdayInterval :: Rule
ruleHourofdayHourofdayInterval = Rule
  { name = "<hour-of-day> - <hour-of-day> (interval)"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "-|do|aż po|po"
    , Predicate $ and . sequence [isNotLatent, isATimeOfDay]
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleTimeBeforeLast :: Rule
ruleTimeBeforeLast = Rule
  { name = "<time> before last"
  , pattern =
    [ dimension Time
    , regex "przed ?ostatni(ego|ch|emu|mi|m|(a|ą)|ej)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ predNth (-2) False td
      _ -> Nothing
  }

ruleNamedmonthDayofmonthOrdinal :: Rule
ruleNamedmonthDayofmonthOrdinal = Rule
  { name = "<named-month> <day-of-month> (ordinal)"
  , pattern =
    [ Predicate isAMonth
    , Predicate isDOMOrdinal
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:token:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleChristmasEve :: Rule
ruleChristmasEve = Rule
  { name = "christmas eve"
  , pattern =
    [ regex "(wigilia|wigilii|wigili(e|ę)|wigili(a|ą)|wigilio) ?(bo(z|ż)ego narodzenia)?"
    ]
  , prod = \_ -> tt $ monthDay 12 24
  }

ruleInduringThePartofday :: Rule
ruleInduringThePartofday = Rule
  { name = "in|during the <part-of-day>"
  , pattern =
    [ regex "(w|na) ?(czas(ie)?)"
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ notLatent td
      _ -> Nothing
  }

ruleThanksgivingDay :: Rule
ruleThanksgivingDay = Rule
  { name = "thanksgiving day"
  , pattern =
    [ regex "((s|ś)wiet(a|o)|(dzie(n|ń)))? ?dzi(e|ę)kczynieni(e|a)"
    ]
  , prod = \_ -> tt $ nthDOWOfMonth 4 4 11
  }

ruleDayofmonthordinalNamedmonth :: Rule
ruleDayofmonthordinalNamedmonth = Rule
  { name = "<day-of-month>(ordinal) <named-month>"
  , pattern =
    [ Predicate isDOMOrdinal
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (token:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleIntersectBy :: Rule
ruleIntersectBy = Rule
  { name = "intersect by \",\""
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

ruleNthTimeAfterTime :: Rule
ruleNthTimeAfterTime = Rule
  { name = "nth <time> after <time>"
  , pattern =
    [ dimension Ordinal
    , dimension Time
    , regex "po"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal od:Token Time td1:_:Token Time td2:_) ->
        tt $ predNthAfter (TOrdinal.value od - 1) td1 td2
      _ -> Nothing
  }

ruleMmdd :: Rule
ruleMmdd = Rule
  { name = "mm/dd"
  , pattern =
    [ regex "(0?[1-9]|1[0-2])/(3[01]|[12]\\d|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (mm:dd:_)):_) -> do
        m <- parseInt mm
        d <- parseInt dd
        tt $ monthDay m d
      _ -> Nothing
  }

ruleAfterDuration :: Rule
ruleAfterDuration = Rule
  { name = "after <duration>"
  , pattern =
    [ regex "po"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) ->
        tt . notLatent . withDirection TTime.After $ inDuration dd
      _ -> Nothing
  }

ruleDayofmonthOrdinalOfNamedmonth :: Rule
ruleDayofmonthOrdinalOfNamedmonth = Rule
  { name = "<day-of-month> (ordinal) of <named-month>"
  , pattern =
    [ Predicate isDOMOrdinal
    , regex "of|in"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleFromTimeofdayTimeofdayInterval :: Rule
ruleFromTimeofdayTimeofdayInterval = Rule
  { name = "from <time-of-day> - <time-of-day> (interval)"
  , pattern =
    [ regex "(niż|niz|od)"
    , Predicate isATimeOfDay
    , regex "((but )?before)|\\-|do|po|aż do|az do|aż po|az po"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleExactlyTimeofday :: Rule
ruleExactlyTimeofday = Rule
  { name = "exactly <time-of-day>"
  , pattern =
    [ regex "(r(o|ó)wno|dok(l|ł)adnie)( o)?"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleSeason3 :: Rule
ruleSeason3 = Rule
  { name = "season"
  , pattern =
    [ regex "zima|zimy|zimie|zimę|zime|zimą|zima|zimie|zimo"
    ]
  , prod = \_ ->
      let from = monthDay 12 21
          to = monthDay 3 20
      in Token Time <$> interval TTime.Open from to
  }

ruleSeason :: Rule
ruleSeason = Rule
  { name = "season"
  , pattern =
    [ regex "lato|lata|latu|latem|lecie"
    ]
  , prod = \_ ->
      let from = monthDay 6 21
          to = monthDay 9 23
      in Token Time <$> interval TTime.Open from to
  }

ruleIntegerLatentTimeofday :: Rule
ruleIntegerLatentTimeofday = Rule
  { name = "<integer> (latent time-of-day)"
  , pattern =
    [ Predicate $ isIntegerBetween 0 23
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        v <- getIntValue token
        tt . mkLatent $ hour True v
      _ -> Nothing
  }

ruleBetweenDatetimeAndDatetimeInterval :: Rule
ruleBetweenDatetimeAndDatetimeInterval = Rule
  { name = "between <datetime> and <datetime> (interval)"
  , pattern =
    [ regex "(po|po )?mi(e|ę)dzy"
    , dimension Time
    , regex "a|i"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleNewYearsEve :: Rule
ruleNewYearsEve = Rule
  { name = "new year's eve"
  , pattern =
    [ regex "sylwester|nowy rok"
    ]
  , prod = \_ -> tt $ monthDay 12 31
  }

ruleDurationAgo :: Rule
ruleDurationAgo = Rule
  { name = "<duration> ago"
  , pattern =
    [ dimension Duration
    , regex "temu"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_) ->
        tt $ durationAgo dd
      _ -> Nothing
  }

ruleByTheEndOfTime :: Rule
ruleByTheEndOfTime = Rule
  { name = "by the end of <time>"
  , pattern =
    [ regex "do (ko[ńn]ca )?(tego)?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> interval TTime.Closed now td
      _ -> Nothing
  }

ruleDaybeforeyesterdaySingleword :: Rule
ruleDaybeforeyesterdaySingleword = Rule
  { name = "day-before-yesterday (single-word)"
  , pattern =
    [ regex "przedwczoraj"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 2
  }

ruleLastNCycle :: Rule
ruleLastNCycle = Rule
  { name = "last n <cycle>"
  , pattern =
    [ regex "ostatni(ego|ch|emu|mi|m|(a|ą)|ej|e)?|(po ?)?przedni(ego|ch|emu|mi|m|e|(a|ą)|ej)?"
    , Predicate $ isIntegerBetween 1 9999
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token TimeGrain grain:_) -> do
        v <- getIntValue token
        tt $ cycleN True grain (- v)
      _ -> Nothing
  }

ruleTimeofdaySharp :: Rule
ruleTimeofdaySharp = Rule
  { name = "<time-of-day> sharp"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "r(o|ó)wno|dok(l|ł)adnie"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleWithinDuration :: Rule
ruleWithinDuration = Rule
  { name = "within <duration>"
  , pattern =
    [ regex "(w )?ci(a|ą)gu|zakresie|obrębie|obrebie"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) ->
        Token Time <$> interval TTime.Open now (inDuration dd)
      _ -> Nothing
  }

ruleTimeofdayRano :: Rule
ruleTimeofdayRano = Rule
  { name = "<time-of-day> rano"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "(z )?ran(o|a|u|em)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ timeOfDayAMPM True td
      _ -> Nothing
  }

ruleDayofmonthNonOrdinalNamedmonth :: Rule
ruleDayofmonthNonOrdinalNamedmonth = Rule
  { name = "<day-of-month> (non ordinal) <named-month>"
  , pattern =
    [ Predicate isDOMInteger
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (token:Token Time td:_) -> Token Time <$> intersectDOM td token
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
      (Token Time td1:Token Time td2:_) -> Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleAboutTimeofday :: Rule
ruleAboutTimeofday = Rule
  { name = "about <time-of-day>"
  , pattern =
    [ regex "o?ko(l|ł)o|mniej wi(e|ę)cej|tak o"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleUntilTimeofday :: Rule
ruleUntilTimeofday = Rule
  { name = "until <time-of-day>"
  , pattern =
    [ regex "(a(ż|z) )?do|przed"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt . notLatent $ withDirection TTime.Before td
      _ -> Nothing
  }

ruleAtTimeofday :: Rule
ruleAtTimeofday = Rule
  { name = "at <time-of-day>"
  , pattern =
    [ regex "o|na|@"
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
    , dimension Time
    , regex "w(e)?|z(e)?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal od:Token Time td1:_:Token Time td2:_) -> Token Time .
        predNth (TOrdinal.value od - 1) False <$> intersect td2 td1
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
    [ regex "((wek|week|wik)(\\s|-)?end|wkend)"
    ]
  , prod = \_ -> tt weekend
  }

ruleEomendOfMonth :: Rule
ruleEomendOfMonth = Rule
  { name = "EOM|End of month"
  , pattern =
    [ regex "(na |w )?(koniec|ko(n|ń)ca|ko(n|ń)cu|ko(n|ń)cowi|ko(n|ń)cem|ko(n|ń)c(o|ó)wke) (miesi(a|ą)ca|msc)"
    ]
  , prod = \_ -> tt $ cycleNth TG.Month 1
  }

ruleNextTime :: Rule
ruleNextTime = Rule
  { name = "next <time>"
  , pattern =
    [ regex "kolejn(ym|y|ego|emu|(a|ą)|ej|e)|nast(e|ę)pn(ym|y|ego|emu|e|(a|ą)|ej|e)|przysz(l|ł)(ego|emu|ym|(a|ą)|ej|ych|i|ymi|y|e)"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ predNth 0 True td
      _ -> Nothing
  }

ruleDayaftertomorrowSingleword :: Rule
ruleDayaftertomorrowSingleword = Rule
  { name = "day-after-tomorrow (single-word)"
  , pattern =
    [ regex "(po ?jutr(o|ze))"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 2
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
    [ regex "(\\d{2,4})-(0?[1-9]|1[0-2])-(3[01]|[12]\\d|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (yy:mm:dd:_)):_) -> do
        y <- parseInt yy
        m <- parseInt mm
        d <- parseInt dd
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

ruleTheOrdinalCycleAfterTime :: Rule
ruleTheOrdinalCycleAfterTime = Rule
  { name = "the <ordinal> <cycle> after <time>"
  , pattern =
    [ regex "the"
    , dimension Ordinal
    , dimension TimeGrain
    , regex "after"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Ordinal od:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter True grain (TOrdinal.value od - 1) td
      _ -> Nothing
  }

ruleIntersectByOfFromS :: Rule
ruleIntersectByOfFromS = Rule
  { name = "intersect by \"of\", \"from\", \"'s\""
  , pattern =
    [ Predicate isNotLatent
    , regex "z"
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
    [ regex "kolejn(ym|y|ego|emu|(a|ą)|ej|e)|nast(e|ę)pn(ym|y|ego|emu|(a|ą)|ej|e)"
    , Predicate $ isIntegerBetween 1 9999
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token TimeGrain grain:_) -> do
        v <- getIntValue token
        tt $ cycleN True grain v
      _ -> Nothing
  }

ruleMorning :: Rule
ruleMorning = Rule
  { name = "morning"
  , pattern =
    [ regex "rano|poran(ek|ku|ka)|z rana|(s|ś)witem"
    ]
  , prod = \_ ->
      let from = hour False 4
          to = hour False 12
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleThisPartofday :: Rule
ruleThisPartofday = Rule
  { name = "this <part-of-day>"
  , pattern =
    [ regex "ten|tego|ta|to"
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
    [ regex "te(mu|n|go|j)|tym|t(a|ą)|nadchodz(a|ą)c(ym|y|ego|emu|(a|ą)|ej)|obecn(ym|y|emu|ego|nym|(a|ą)|ej)"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) ->
        tt $ cycleNth grain 0
      _ -> Nothing
  }

ruleThisTime :: Rule
ruleThisTime = Rule
  { name = "this <time>"
  , pattern =
    [ regex "te(mu|n|go|j)|ta|to|tym|nadchodz(a|ą)c(ym|y|ego|emu|(a|ą)|ej)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ predNth 0 False td
      _ -> Nothing
  }

ruleDurationHence :: Rule
ruleDurationHence = Rule
  { name = "<duration> hence"
  , pattern =
    [ dimension Duration
    , regex "p(o|ó)(z|ź)niej|potem"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_) ->
        tt $ inDuration dd
      _ -> Nothing
  }

ruleDayofmonthNonOrdinalOfNamedmonth :: Rule
ruleDayofmonthNonOrdinalOfNamedmonth = Rule
  { name = "<day-of-month> (non ordinal) of <named-month>"
  , pattern =
    [ Predicate isDOMInteger
    , regex "of|in"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleNthTimeTime :: Rule
ruleNthTimeTime = Rule
  { name = "nth <time> <time>"
  , pattern =
    [ dimension Ordinal
    , dimension Time
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal od:Token Time td1:Token Time td2:_) -> Token Time .
        predNth (TOrdinal.value od - 1) False <$> intersect td2 td1
      _ -> Nothing
  }

ruleTimeofdayPopoudniuwieczoremwNocy :: Rule
ruleTimeofdayPopoudniuwieczoremwNocy = Rule
  { name = "<time-of-day> popołudniu/wieczorem/w nocy"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "(w )?noc(y|(a|ą))|(po ?)?po(l|ł)udni(em|e|a|u)|wiecz(o|ó)r(i|u|a|owi|em|rze)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ timeOfDayAMPM False td
      _ -> Nothing
  }

ruleOnANamedday :: Rule
ruleOnANamedday = Rule
  { name = "on a named-day"
  , pattern =
    [ regex "we?"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:x:_) -> Just x
      _ -> Nothing
  }

ruleYearLatent :: Rule
ruleYearLatent = Rule
  { name = "year (latent)"
  , pattern =
    [ Predicate $ isIntegerBetween (- 10000) 999
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
         n <- getIntValue token
         tt . mkLatent $ year n
      _ -> Nothing
  }

ruleYesterday :: Rule
ruleYesterday = Rule
  { name = "yesterday"
  , pattern =
    [ regex "wczoraj(szym|szy)?|wczrj|wczor"
    ]
  , prod = \_ -> tt . cycleNth TG.Day $ - 1
  }

ruleSeason2 :: Rule
ruleSeason2 = Rule
  { name = "season"
  , pattern =
    [ regex "jesień|jesien|jesieni|jesienią|jesienia"
    ]
  , prod = \_ ->
      let from = monthDay 9 23
          to = monthDay 12 21
      in Token Time <$> interval TTime.Open from to
  }

ruleAfterTimeofday :: Rule
ruleAfterTimeofday = Rule
  { name = "after <time-of-day>"
  , pattern =
    [ regex "po"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ withDirection TTime.After td
      _ -> Nothing
  }

ruleChristmas :: Rule
ruleChristmas = Rule
  { name = "christmas"
  , pattern =
    [ regex "((Ś|ś|s)wi(e|ę)ta)? ?bo(z|ż)(ym|ego|e) narodzeni(e|a|u)"
    ]
  , prod = \_ -> tt $ monthDay 12 25
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

ruleOrdinalCycleAfterTime :: Rule
ruleOrdinalCycleAfterTime = Rule
  { name = "<ordinal> <cycle> after <time>"
  , pattern =
    [ dimension Ordinal
    , dimension TimeGrain
    , regex "after"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal od:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter True grain (TOrdinal.value od - 1) td
      _ -> Nothing
  }

ruleOrdinalCycleOfTime :: Rule
ruleOrdinalCycleOfTime = Rule
  { name = "<ordinal> <cycle> of <time>"
  , pattern =
    [ dimension Ordinal
    , dimension TimeGrain
    , regex "w(e)?|z(e)?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal od:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter True grain (TOrdinal.value od - 1) td
      _ -> Nothing
  }

ruleHhmm :: Rule
ruleHhmm = Rule
  { name = "hh:mm"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3]))[:.]([0-5]\\d)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (hh:mm:_)):_) -> do
        h <- parseInt hh
        m <- parseInt mm
        tt $ hourMinute True h m
      _ -> Nothing
  }

ruleFromHourofdayHourofdayInterval :: Rule
ruleFromHourofdayHourofdayInterval = Rule
  { name = "from <hour-of-day> - <hour-of-day> (interval)"
  , pattern =
    [ regex "od"
    , Predicate isATimeOfDay
    , regex "-|to|th?ru|through|until"
    , Predicate $ and . sequence [isNotLatent, isATimeOfDay]
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleYear :: Rule
ruleYear = Rule
  { name = "year"
  , pattern =
    [ Predicate $ isIntegerBetween 1000 2100
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt $ year n
      _ -> Nothing
  }

ruleHalloweenDay :: Rule
ruleHalloweenDay = Rule
  { name = "halloween day"
  , pattern =
    [ regex "hall?owe?en( day)?"
    ]
  , prod = \_ -> tt $ monthDay 10 31
  }

ruleNamedmonthDayofmonthNonOrdinal :: Rule
ruleNamedmonthDayofmonthNonOrdinal = Rule
  { name = "<named-month> <day-of-month> (non ordinal)"
  , pattern =
    [ Predicate isAMonth
    , Predicate isDOMInteger
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:token:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleHhmmMilitary :: Rule
ruleHhmmMilitary = Rule
  { name = "hhmm (military)"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3]))([0-5]\\d)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (hh:mm:_)):_) -> do
        h <- parseInt hh
        m <- parseInt mm
        tt . mkLatent $ hourMinute False h m
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

ruleLastDayofweekOfTime :: Rule
ruleLastDayofweekOfTime = Rule
  { name = "last <day-of-week> of <time>"
  , pattern =
    [ regex "ostatni(ego|ch|emu|mi|m|(a|ą)|ej)?"
    , Predicate isADayOfWeek
    , regex "w(e)?|z(e)?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        tt $ predLastOf td1 td2
      _ -> Nothing
  }

ruleFathersDay :: Rule
ruleFathersDay = Rule
  { name = "Father's Day"
  , pattern =
    [ regex "dzie(n|ń) ?(taty|ojca)"
    ]
  , prod = \_ -> tt $ nthDOWOfMonth 3 7 6
  }

ruleLastCycleTime :: Rule
ruleLastCycleTime = Rule
  { name = "last <cycle> <time>"
  , pattern =
    [ regex "ostatni(ego|ch|emu|mi|m|(a|ą)|ej)?"
    , dimension TimeGrain
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:Token Time td:_) ->
        tt $ cycleLastOf grain td
      _ -> Nothing
  }

ruleByTime :: Rule
ruleByTime = Rule
  { name = "by <time>"
  , pattern =
    [ regex "(a(z|ż) )?do"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> interval TTime.Open now td
      _ -> Nothing
  }

ruleHhmmMilitaryAmpm :: Rule
ruleHhmmMilitaryAmpm = Rule
  { name = "hhmm (military) am|pm"
  , pattern =
    [ regex "((?:1[012]|0?\\d))([0-5]\\d)"
    , regex "([ap])\\.?m?\\.?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (hh:mm:_)):
       Token RegexMatch (GroupMatch (ap:_)):
       _) -> do
        h <- parseInt hh
        m <- parseInt mm
        tt . timeOfDayAMPM (Text.toLower ap == "a") $ hourMinute True h m
      _ -> Nothing
  }

ruleCycleBeforeTime :: Rule
ruleCycleBeforeTime = Rule
  { name = "<cycle> before <time>"
  , pattern =
    [ dimension TimeGrain
    , regex "przed"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter False grain (-1) td
      _ -> Nothing
  }

ruleTimeofdayTimeofdayInterval :: Rule
ruleTimeofdayTimeofdayInterval = Rule
  { name = "<time-of-day> - <time-of-day> (interval)"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isATimeOfDay]
    , regex "\\-|:|do|po|aż do|az do|aż po|az po"
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
    , regex "po"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_:Token Time td:_) ->
        tt $ durationAfter dd td
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
      (token:_) -> do
        n <- getIntValue token
        tt . cycleNthAfter True TG.Quarter (n - 1) $
          cycleNth TG.Year 0
      _ -> Nothing
  }

ruleDurationBeforeTime :: Rule
ruleDurationBeforeTime = Rule
  { name = "<duration> before <time>"
  , pattern =
    [ dimension Duration
    , regex "do|przed"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_:Token Time td:_) ->
        tt $ durationBefore dd td
      _ -> Nothing
  }

ruleMmddyyyy :: Rule
ruleMmddyyyy = Rule
  { name = "mm/dd/yyyy"
  , pattern =
    [ regex "(0?[1-9]|1[0-2])[/-](3[01]|[12]\\d|0?[1-9])[-/](\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (mm:dd:yy:_)):_) -> do
        y <- parseInt yy
        m <- parseInt mm
        d <- parseInt dd
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

ruleEoyendOfYear :: Rule
ruleEoyendOfYear = Rule
  { name = "EOY|End of year"
  , pattern =
    [ regex "(na |w )?(koniec|ko(n|ń)ca|ko(n|ń)cu|ko(n|ń)cowi|ko(n|ń)cem|ko(n|ń)c(o|ó)wke) (rok(u|owi|iem))"
    ]
  , prod = \_ -> tt $ cycleNth TG.Year 1
  }

ruleTomorrow :: Rule
ruleTomorrow = Rule
  { name = "tomorrow"
  , pattern =
    [ regex "jutr(o|a|u|em|ze(jszy|jsza)?)|jtr|jutr"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 1
  }

ruleMothersDay :: Rule
ruleMothersDay = Rule
  { name = "Mother's Day"
  , pattern =
    [ regex "dzie(n|ń) ?ma(my|tki|m)"
    ]
  , prod = \_ -> tt $ nthDOWOfMonth 2 7 5
  }

ruleTimeofdayOclock :: Rule
ruleTimeofdayOclock = Rule
  { name = "<time-of-day> o'clock"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "godzina"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ notLatent td
      _ -> Nothing
  }

ruleDayofmonthordinalNamedmonthYear :: Rule
ruleDayofmonthordinalNamedmonthYear = Rule
  { name = "<day-of-month>(ordinal) <named-month> year"
  , pattern =
    [ Predicate isDOMOrdinal
    , Predicate isAMonth
    , regex "(\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (token:
       Token Time td:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> do
         y <- parseInt match
         dom <- intersectDOM td token
         Token Time <$> intersect dom (year y)
      _ -> Nothing
  }

ruleHhmmss :: Rule
ruleHhmmss = Rule
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
  [ ruleAboutTimeofday
  , ruleAbsorptionOfAfterNamedDay
  , ruleAfterDuration
  , ruleAfterTimeofday
  , ruleAfternoon
  , ruleAtTimeofday
  , ruleBetweenDatetimeAndDatetimeInterval
  , ruleBetweenTimeofdayAndTimeofdayInterval
  , ruleByTheEndOfTime
  , ruleByTime
  , ruleChristmas
  , ruleChristmasEve
  , ruleCycleAfterTime
  , ruleCycleBeforeTime
  , ruleDatetimeDatetimeInterval
  , ruleDayaftertomorrowSingleword
  , ruleDaybeforeyesterdaySingleword
  , ruleDayofmonthNonOrdinalNamedmonth
  , ruleDayofmonthNonOrdinalOfNamedmonth
  , ruleDayofmonthOrdinal
  , ruleDayofmonthOrdinalOfNamedmonth
  , ruleDayofmonthordinalNamedmonth
  , ruleDayofmonthordinalNamedmonthYear
  , ruleDurationAfterTime
  , ruleDurationAgo
  , ruleDurationBeforeTime
  , ruleDurationFromNow
  , ruleDurationHence
  , ruleEomendOfMonth
  , ruleEoyendOfYear
  , ruleEveningnight
  , ruleExactlyTimeofday
  , ruleFathersDay
  , ruleFromDatetimeDatetimeInterval
  , ruleFromHourofdayHourofdayInterval
  , ruleFromTimeofdayTimeofdayInterval
  , ruleHalloweenDay
  , ruleHhmm
  , ruleHhmmMilitary
  , ruleHhmmMilitaryAmpm
  , ruleHhmmss
  , ruleHourofdayHourofdayInterval
  , ruleHourofdayIntegerAsRelativeMinutes
  , ruleHourofdayQuarter
  , ruleHourofdayHalf
  , ruleInDuration
  , ruleInduringThePartofday
  , ruleIntegerLatentTimeofday
  , ruleIntersect
  , ruleIntersectBy
  , ruleIntersectByOfFromS
  , ruleLastCycle
  , ruleLastCycleOfTime
  , ruleLastCycleTime
  , ruleLastDayofweekOfTime
  , ruleLastDayofweekTime
  , ruleLastNCycle
  , ruleLastTime
  , ruleLunch
  , ruleMmdd
  , ruleMmddyyyy
  , ruleMonthDdddInterval
  , ruleMorning
  , ruleMothersDay
  , ruleNamedmonthDayofmonthNonOrdinal
  , ruleNamedmonthDayofmonthOrdinal
  , ruleNewYearsEve
  , ruleNextCycle
  , ruleNextNCycle
  , ruleNextTime
  , ruleNoon
  , ruleNow
  , ruleNthTimeAfterTime
  , ruleNthTimeOfTime
  , ruleNthTimeTime
  , ruleOnANamedday
  , ruleOnDate
  , ruleOrdinalAsHour
  , ruleOrdinalCycleAfterTime
  , ruleOrdinalCycleOfTime
  , ruleOrdinalCycleTime
  , ruleOrdinalQuarter
  , ruleOrdinalQuarterYear
  , rulePolishIndependenceDay
  , ruleRelativeMinutesAfterpastIntegerHourofday
  , ruleQuarterAfterpastIntegerHourofday
  , ruleHalfAfterpastIntegerHourofday
  , ruleRelativeMinutesTotillbeforeIntegerHourofday
  , ruleQuarterTotillbeforeIntegerHourofday
  , ruleHalfTotillbeforeIntegerHourofday
  , ruleSeason
  , ruleSeason2
  , ruleSeason3
  , ruleSeason4
  , ruleThanksgivingDay
  , ruleTheCycleOfTime
  , ruleTheIdesOfNamedmonth
  , ruleTheOrdinalCycleAfterTime
  , ruleThisCycle
  , ruleThisPartofday
  , ruleThisTime
  , ruleThisnextDayofweek
  , ruleTimeAfterNext
  , ruleTimeBeforeLast
  , ruleTimePartofday
  , ruleTimeofdayApproximately
  , ruleTimeofdayOclock
  , ruleTimeofdayPopoudniuwieczoremwNocy
  , ruleTimeofdayRano
  , ruleTimeofdaySharp
  , ruleTimeofdayTimeofdayInterval
  , ruleToday
  , ruleTomorrow
  , ruleUntilTimeofday
  , ruleValentinesDay
  , ruleWeekend
  , ruleWithinDuration
  , ruleYear
  , ruleYearLatent
  , ruleYearLatent2
  , ruleYesterday
  , ruleYyyymmdd
  , ruleTimezone
  ]
  ++ ruleDaysOfWeek
  ++ ruleMonths
