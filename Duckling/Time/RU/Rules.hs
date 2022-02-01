-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.RU.Rules
  ( rules
  ) where

import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers (isGrain)
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Types (OrdinalData(..))
import Duckling.Regex.Types
import Duckling.Time.Helpers
import Duckling.Time.Types (TimeData(..))
import Duckling.Types
import qualified Duckling.Ordinal.Types as TOrdinal
import qualified Duckling.Time.Types as TTime
import qualified Duckling.TimeGrain.Types as TG

ruleInstants :: [Rule]
ruleInstants = mkRuleInstants
  [ ( "сейчас"       , TG.Second ,  0, "сейчас"               )
  , ( "сегодня"      , TG.Day    ,  0, "сегодня"              )
  , ( "завтра"       , TG.Day    ,  1, "завтра"               )
  , ( "вчера"        , TG.Day    , -1, "вчера"                )
  , ( "послезавтра"  , TG.Day    ,  2, "послезавтра"          )
  , ( "послепослезавтра", TG.Day ,  3, "послепослезавтра"     )
  , ( "позавчера"    , TG.Day    , -2, "позавчера"            )
  , ( "позапозавчера", TG.Day    , -3, "позапозавчера"        )
  , ( "Конец месяца" , TG.Month  ,  1, "(конец|конца) месяца" )
  , ( "Конец года"   , TG.Year   ,  1, "(конец|конца) года"   )
  ]

ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = mkRuleDaysOfWeek
  [ ( "Понедельник" , "понедельник(а)?|пн" )
  , ( "Вторник"     , "вторник(а)?|вт"     )
  , ( "Среда"       , "сред(а|у)|ср"       )
  , ( "Четверг"     , "четверг(а)?|чт"     )
  , ( "Пятница"     , "пятниц(а|у)|пт"     )
  , ( "Суббота"     , "суббот(а|у)|сб"     )
  , ( "Воскресенье" , "воскресенье|вс"     )
  ]

ruleMonths :: [Rule]
ruleMonths = mkRuleMonths
  [ ( "Январь"   , "январ(ь|я)|янв\\.?"   )
  , ( "Февраль"  , "феврал(ь|я)|фев\\.?"  )
  , ( "Март"     , "март(а)?|мар\\.?"     )
  , ( "Апрель"   , "апрел(ь|я)|апр\\.?"   )
  , ( "Май"      , "ма(й|я)"              )
  , ( "Июнь"     , "июн(ь|я)|июн\\.?"     )
  , ( "Июль"     , "июл(ь|я)|июл\\.?"     )
  , ( "Август"   , "август(а)?|авг\\.?"   )
  , ( "Сентябрь" , "сентябр(ь|я)|сен\\.?" )
  , ( "Октябрь"  , "октябр(ь|я)|окт\\.?"  )
  , ( "Ноябрь"   , "ноябр(ь|я)?|ноя\\.?"  )
  , ( "Декабрь"  , "декабр(ь|я)|дек\\.?"  )
  ]

ruleSeasons :: [Rule]
ruleSeasons = mkRuleSeasons
  [ ( "лето"  , "лет(о|а)"  , monthDay  6  1, monthDay  8 31 )
  , ( "осень" , "осень" , monthDay  9  1, monthDay 11 30 )
  , ( "зима"  , "зима"  , monthDay 12  1, monthDay  2 28 )
  , ( "весна" , "весна" , monthDay  3  1, monthDay  5 31 )
  ]

ruleHolidays :: [Rule]
ruleHolidays = mkRuleHolidays
  [ ( "Новый год"                  , "Новый год"                   , monthDay  1  1 )
  , ( "Рождество Христово"         , "рождество( христово)?"       , monthDay  1  7 )
  , ( "День защитника отечества"   , "день защитника"              , monthDay  2 23 )
  , ( "Международный женский день" , "международный женский день"  , monthDay  3  8 )
  , ( "День смеха"                 , "день смеха"                  , monthDay  4  1 )
  , ( "Праздник Весны и Труда"     , "день труда"                  , monthDay  5  1 )
  , ( "День Победы"                , "День Победы"                 , monthDay  5  9 )
  , ( "День России"                , "день россии"                 , monthDay  6 12 )
  , ( "День народного единства"    , "день единства"               , monthDay  11 4 )
  ]

ruleLastTime :: Rule
ruleLastTime = Rule
  { name = "last <time>"
  , pattern =
    [ regex "(на |в )?прошл(ый|ого|ому|ом|ое|ые|ых|ыми|ым|ая|ой|ую)"
    , Predicate isOkWithThisNext
    ]
  , prod = \case
      (_:Token Time td:_) ->
        tt $ predNth (-1) False td
      _ -> Nothing
  }

ruleDatetimeDatetimeInterval :: Rule
ruleDatetimeDatetimeInterval = Rule
  { name = "<datetime> - <datetime> (interval)"
  , pattern =
    [ Predicate isNotLatent
    , regex "\\-"
    , Predicate isNotLatent
    ]
  , prod = \case
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleDateDateInterval :: Rule
ruleDateDateInterval = Rule
  { name = "dd.(mm.)? - dd.mm.(yy[yy]?)? (interval)"
  , pattern =
    [ regex "(?:с\\s+)?(10|20|30|31|[012]?[1-9])\\.?((?<=\\.)(?:10|11|12|0?[1-9])(?:\\.?))?"
    , regex "\\-|/|по"
    , regex "(10|20|30|31|[012]?[1-9])\\.(10|11|12|0?[1-9])\\.?((?<=\\.)\\d{2,4})?"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (d1:"":_)):
       _:
       Token RegexMatch (GroupMatch (d2:m2:"":_)):
       _) -> do
          d1 <- parseInt d1
          d2 <- parseInt d2
          m2 <- parseInt m2
          Token Time <$> interval TTime.Closed (monthDay m2 d1) (monthDay m2 d2)
      (Token RegexMatch (GroupMatch (d1:"":_)):
       _:
       Token RegexMatch (GroupMatch (d2:m2:y:_)):
       _) -> do
          d1 <- parseInt d1
          d2 <- parseInt d2
          m2 <- parseInt m2
          y <- parseInt y
          Token Time <$> interval TTime.Closed (yearMonthDay y m2 d1) (yearMonthDay y m2 d2)
      (Token RegexMatch (GroupMatch (d1:m1:_)):
       _:
       Token RegexMatch (GroupMatch (d2:m2:"":_)):
       _) -> do
          d1 <- parseInt d1
          d2 <- parseInt d2
          m1 <- parseInt m1
          m2 <- parseInt m2
          Token Time <$> interval TTime.Closed (monthDay m1 d1) (monthDay m2 d2)
      (Token RegexMatch (GroupMatch (d1:m1:_)):
       _:
       Token RegexMatch (GroupMatch (d2:m2:y:_)):
       _) -> do
          d1 <- parseInt d1
          d2 <- parseInt d2
          m1 <- parseInt m1
          m2 <- parseInt m2
          y <- parseInt y
          Token Time <$> interval TTime.Closed (yearMonthDay y m1 d1) (yearMonthDay y m2 d2)
      _ -> Nothing
  }

ruleInDuration :: Rule
ruleInDuration = Rule
  { name = "in <duration>"
  , pattern =
    [ regex "через"
    , dimension Duration
    ]
  , prod = \case
      (_:Token Duration dd:_) ->
        tt $ inDuration dd
      _ -> Nothing
  }

ruleLastCycleOfTime :: Rule
ruleLastCycleOfTime = Rule
  { name = "last <cycle> of <time>"
  , pattern =
    [ regex "последн(ий|юю|яя|его|ему)"
    , dimension TimeGrain
    , regex "в"
    , dimension Time
    ]
  , prod = \case
      (_:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleLastOf grain td
      _ -> Nothing
  }

ruleLastCycleOfTime2 :: Rule
ruleLastCycleOfTime2 = Rule
  { name = "last <cycle> of <time>"
  , pattern =
    [ regex "последн(ий|юю|яя|его|ему)"
    , dimension TimeGrain
    , dimension Time
    ]
  , prod = \case
      (_:Token TimeGrain grain:Token Time td:_) ->
        tt $ cycleLastOf grain td
      _ -> Nothing
  }

ruleMonthDdddInterval :: Rule
ruleMonthDdddInterval = Rule
  { name = "<month> dd-dd (interval)"
  , pattern =
    [ regex "(?:с\\s+)?([012]?\\d|30|31)(го|\\.)?"
    , regex "\\-|по|до"
    , regex "([012]?\\d|30|31)(ое|\\.)?"
    , Predicate isAMonth
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (m1:_)):
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

ruleYearLatent2 :: Rule
ruleYearLatent2 = Rule
  { name = "year (latent)"
  , pattern =
    [ Predicate $ isIntegerBetween 2101 10000
    ]
  , prod = \case
      (token:_) -> do
        v <- getIntValue token
        tt . mkLatent $ year v
      _ -> Nothing
  }

ruleTimeAfterNext :: Rule
ruleTimeAfterNext = Rule
  { name = "<time> after next"
  , pattern =
    [ dimension Time
    , regex "после следующ(ей|его)"
    ]
  , prod = \case
      (Token Time td:_:_) ->
        tt $ predNth 1 True td
      _ -> Nothing
  }

ruleNoon :: Rule
ruleNoon = Rule
  { name = "noon"
  , pattern =
    [ regex "полдень"
    ]
  , prod = \_ -> tt $ hour False 12
  }

ruleMidnight :: Rule
ruleMidnight = Rule
  { name = "midnight"
  , pattern =
    [ regex "полночь"
    ]
  , prod = \_ -> tt $ hour False 0
  }

ruleThisnextDayofweek :: Rule
ruleThisnextDayofweek = Rule
  { name = "this|next <day-of-week>"
  , pattern =
    [ regex "след(ующ(ий|ая|ее|ую))?"
    , Predicate isADayOfWeek
    ]
  , prod = \case
      (_:Token Time td:_) ->
        tt $ predNth 0 True td
      _ -> Nothing
  }

ruleBetweenTimeofdayAndTimeofdayInterval :: Rule
ruleBetweenTimeofdayAndTimeofdayInterval = Rule
  { name = "between <time-of-day> and <time-of-day> (interval)"
  , pattern =
    [ regex "с"
    , Predicate isATimeOfDay
    , regex "по"
    , Predicate isATimeOfDay
    ]
  , prod = \case
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleNextCycle :: Rule
ruleNextCycle = Rule
  { name = "next <cycle>"
  , pattern =
    [ regex "(на |в |к )?след(ующ(ий|его|ему|ими|ем|ие|их|им|ей|ая|ую))?"
    , dimension TimeGrain
    ]
  , prod = \case
      (_:Token TimeGrain grain:_) ->
        tt $ cycleNth grain 1
      _ -> Nothing
  }

ruleOnDate :: Rule
ruleOnDate = Rule
  { name = "on <date>"
  , pattern =
    [ regex "во|в|к"
    , dimension Time
    ]
  , prod = \case
      (_:x:_) -> Just x
      _ -> Nothing
  }

ruleLastCycle :: Rule
ruleLastCycle = Rule
  { name = "last <cycle>"
  , pattern =
    [ regex "(на |в )?прошл(ый|ого|ому|ом|ое|ые|ых|ыми|ым|ая|ой|ую)"
    , dimension TimeGrain
    ]
  , prod = \case
      (_:Token TimeGrain grain:_) ->
        tt . cycleNth grain $ - 1
      _ -> Nothing
  }

ruleBeforeLastCycle :: Rule
ruleBeforeLastCycle = Rule
  { name = "one <cycle> before last"
  , pattern =
    [ regex "(на |в )?позапрошл(ый|ого|ому|ыми|ом|ое|ые|ых|ым|ая|ой|ую)"
    , dimension TimeGrain
    ]
  , prod = \case
      (_:Token TimeGrain grain:_) ->
        tt . cycleNth grain $ - 2
      _ -> Nothing
  }

ruleLastCycle2 :: Rule
ruleLastCycle2 = Rule
  { name = "last <cycle>"
  , pattern =
    [ regex "(в )?прошедш(ий|его|ему|ими|ем|их|ие|им)"
    , dimension TimeGrain
    ]
  , prod = \case
      (_:Token TimeGrain grain:_) ->
        tt . cycleNth grain $ - 1
      _ -> Nothing
  }

ruleLunch :: Rule
ruleLunch = Rule
  { name = "lunch"
  , pattern =
    [ regex "(в )?обед"
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
    [ regex "(дня|дн[её]м)|пополудни|после полудня"
    ]
  , prod = \_ ->
      let from = hour False 12
          to = hour False 19
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleEvening :: Rule
ruleEvening = Rule
  { name = "evening"
  , pattern =
    [ regex "вечера|вечером|вечер"
    ]
  , prod = \_ ->
      let from = hour False 18
          to = hour False 0
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleNamedmonthDayofmonthOrdinal :: Rule
ruleNamedmonthDayofmonthOrdinal = Rule
  { name = "<named-month> <day-of-month> (ordinal)"
  , pattern =
    [ Predicate isAMonth
    , Predicate isDOMOrdinal
    ]
  , prod = \case
      (Token Time td:token:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleHourofdayIntegerAsRelativeMinutes :: Rule
ruleHourofdayIntegerAsRelativeMinutes = Rule
  { name = "<hour-of-day> <integer> (as relative minutes)"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isAnHourOfDay]
    , Predicate $ isIntegerBetween 1 59
    ]
  , prod = \case
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       token:
       _) -> do
        n <- getIntValue token
        tt $ hourMinute is12H hours n
      _ -> Nothing
  }

ruleDayofmonthordinalNamedmonth :: Rule
ruleDayofmonthordinalNamedmonth = Rule
  { name = "<day-of-month>(ordinal) <named-month>"
  , pattern =
    [ Predicate isDOMOrdinal
    , Predicate isAMonth
    ]
  , prod = \case
      (token:Token Time td:_) -> Token Time <$> intersectDOM td token
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
  , prod = \case
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
    , regex "после"
    , dimension Time
    ]
  , prod = \case
      (Token Ordinal OrdinalData{TOrdinal.value = v}:
       Token Time td1:
       _:
       Token Time td2:
       _) -> tt $ predNthAfter (v - 1) td1 td2
      _ -> Nothing
  }

ruleMmdd :: Rule
ruleMmdd = Rule
  { name = "mm/dd"
  , pattern =
    [ regex "([012]?[1-9]|10|20|30|31)\\.(10|11|12|0?[1-9])\\.?"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        d <- parseInt m1
        m <- parseInt m2
        tt $ monthDay m d
      _ -> Nothing
  }

ruleTimeofdayLatent :: Rule
ruleTimeofdayLatent = Rule
  { name = "time-of-day (latent)"
  , pattern =
    [ Predicate $ isIntegerBetween 0 23
    ]
  , prod = \case
      (token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ hour (n < 12) n
      _ -> Nothing
  }

ruleDurationAgo :: Rule
ruleDurationAgo = Rule
  { name = "<duration> ago"
  , pattern =
    [ dimension Duration
    , regex "(тому )?назад"
    ]
  , prod = \case
      (Token Duration dd:_:_) ->
        tt $ durationAgo dd
      _ -> Nothing
  }

ruleLastNCycle :: Rule
ruleLastNCycle = Rule
  { name = "last n <cycle>"
  , pattern =
    [ regex "последние"
    , Predicate $ isIntegerBetween 1 9999
    , dimension TimeGrain
    ]
  , prod = \case
      (_:token:Token TimeGrain grain:_) -> do
        n <- getIntValue token
        tt $ cycleN True grain (- n)
      _ -> Nothing
  }

ruleWithinDuration :: Rule
ruleWithinDuration = Rule
  { name = "within <duration>"
  , pattern =
    [ regex "в течение"
    , dimension Duration
    ]
  , prod = \case
      (_:Token Duration dd:_) -> Token Time <$>
        interval TTime.Open now (inDuration dd)
      _ -> Nothing
  }

ruleMidnighteodendOfDay :: Rule
ruleMidnighteodendOfDay = Rule
  { name = "midnight|EOD|end of day"
  , pattern =
    [ regex "кон(ец|ц(а|е)) дня"
    ]
  , prod = \_ -> tt $ hour False 0
  }

ruleDayofmonthNonOrdinalNamedmonth :: Rule
ruleDayofmonthNonOrdinalNamedmonth = Rule
  { name = "<day-of-month> (non ordinal) <named-month>"
  , pattern =
    [ Predicate isDOMInteger
    , Predicate isAMonth
    ]
  , prod = \case
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
  , prod = \case
      (Token Time td1:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleUntilTimeofday :: Rule
ruleUntilTimeofday = Rule
  { name = "until <time-of-day>"
  , pattern =
    [ regex "до"
    , dimension Time
    ]
  , prod = \case
      (_:Token Time td:_) -> tt $ withDirection TTime.Before td
      _ -> Nothing
  }

ruleAtTimeofday :: Rule
ruleAtTimeofday = Rule
  { name = "at <time-of-day>"
  , pattern =
    [ regex "в"
    , Predicate isATimeOfDay
    ]
  , prod = \case
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleNthTimeOfTime :: Rule
ruleNthTimeOfTime = Rule
  { name = "nth <time> of <time>"
  , pattern =
    [ dimension Ordinal
    , dimension Time
    , regex "в"
    , dimension Time
    ]
  , prod = \case
      (Token Ordinal OrdinalData{TOrdinal.value = v}:
       Token Time td1:
       _:
       Token Time td2:
       _) -> Token Time . predNth (v - 1) False <$> intersect td2 td1
      _ -> Nothing
  }

ruleNthTimeOfTime2 :: Rule
ruleNthTimeOfTime2 = Rule
  { name = "nth <time> of <time>"
  , pattern =
    [ dimension Ordinal
    , dimension Time
    , dimension Time
    ]
  , prod = \case
      (Token Ordinal OrdinalData{TOrdinal.value = v}:
       Token Time td1:
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
  , prod = \case
      (Token Time td1:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleWeek :: Rule
ruleWeek = Rule
 { name = "week"
 , pattern =
   [ regex "(эта )?неделя" -- regex "(all|rest of the|the|this) week"
   ]
 , prod = \case
     (Token RegexMatch (GroupMatch (m:_)):_) -> do
       let end = cycleNthAfter True TG.Day (-2) $ cycleNth TG.Week 1
       let match = Text.strip $ Text.toLower m
       period <- case match of
         "эта" -> interval TTime.Closed (cycleNth TG.Week 0) end
         "неделя" -> interval TTime.Open today end
         _ -> Nothing
       let l = case match of { "неделя" -> mkLatent; _ -> id; }
       Just $ Token Time $ l period
     _ -> Nothing
 }

ruleWeekend :: Rule
ruleWeekend = Rule
  { name = "week-end"
  , pattern =
    [ regex "выходные"
    ]
  , prod = \_ -> tt $ mkOkForThisNext weekend
  }

ruleOrdinalQuarterYear :: Rule
ruleOrdinalQuarterYear = Rule
  { name = "<ordinal> quarter <year>"
  , pattern =
    [ dimension Ordinal
    , Predicate $ isGrain TG.Quarter
    , dimension Time
    ]
  , prod = \case
      (Token Ordinal od:_:Token Time td:_) ->
        tt $ cycleNthAfter False TG.Quarter (TOrdinal.value od - 1) td
      _ -> Nothing
  }

ruleYyyymmdd :: Rule
ruleYyyymmdd = Rule
  { name = "yyyy-mm-dd"
  , pattern =
    [ regex "(\\d{2,4})-(0?[1-9]|10|11|12)-([012]?[1-9]|10|20|30|31)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (m1:m2:m3:_)):_) -> do
        y <- parseInt m1
        m <- parseInt m2
        d <- parseInt m3
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

ruleIntersectByOfFromS :: Rule
ruleIntersectByOfFromS = Rule
  { name = "intersect by 'of', 'from', 's"
  , pattern =
    [ Predicate isNotLatent
    , regex "на"
    , Predicate isNotLatent
    ]
  , prod = \case
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleNextNCycle :: Rule
ruleNextNCycle = Rule
  { name = "next n <cycle>"
  , pattern =
    [ regex "следующие"
    , Predicate $ isIntegerBetween 1 9999
    , dimension TimeGrain
    ]
  , prod = \case
      (_:token:Token TimeGrain grain:_) -> do
        v <- getIntValue token
        tt $ cycleN True grain v
      _ -> Nothing
  }

ruleMorning :: Rule
ruleMorning = Rule
  { name = "morning"
  , pattern =
    [ regex "утром|утро|утра"
    ]
  , prod = \_ ->
      let from = hour False 3
          to = hour False 12
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleThisPartofday :: Rule
ruleThisPartofday = Rule
  { name = "this <part-of-day>"
  , pattern =
    [ regex "(в )?(эту|этот|этого|этому|эти|это)"
    , Predicate isAPartOfDay
    ]
  , prod = \case
      (_:Token Time td:_) -> Token Time . partOfDay <$> intersect today td
      _ -> Nothing
  }

ruleThisCycle :: Rule
ruleThisCycle = Rule
  { name = "this <cycle>"
  , pattern =
    [ regex "(в )?(эту|этот|этого|этому|эти)"
    , dimension TimeGrain
    ]
  , prod = \case
      (_:Token TimeGrain grain:_) ->
        tt $ cycleNth grain 0
      _ -> Nothing
  }

ruleThisTime :: Rule
ruleThisTime = Rule
  { name = "this <time>"
  , pattern =
    [ regex "(в )?(эту|этот|этого|этому|эти|это)"
    , Predicate isOkWithThisNext
    ]
  , prod = \case
      (_:Token Time td:_) ->
        tt $ predNth 0 False td
      _ -> Nothing
  }

ruleYearLatent :: Rule
ruleYearLatent = Rule
  { name = "year (latent)"
  , pattern =
    [ Predicate $ isIntegerBetween 25 999
    ]
  , prod = \case
      (token:_) -> do
        y <- getIntValue token
        tt . mkLatent $ year y
      _ -> Nothing
  }

ruleAfterTimeofday :: Rule
ruleAfterTimeofday = Rule
  { name = "after <time-of-day>"
  , pattern =
    [ regex "после"
    , dimension Time
    ]
  , prod = \case
      (_:Token Time td:_) -> tt $ withDirection TTime.After td
      _ -> Nothing
  }

ruleNight :: Rule
ruleNight = Rule
  { name = "night"
  , pattern =
    [ regex "ночью|ночи|ночь"
    ]
  , prod = \_ ->
      let from = hour False 0
          to = hour False 4
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleDayofmonthOrdinal :: Rule
ruleDayofmonthOrdinal = Rule
  { name = "<day-of-month> (ordinal)"
  , pattern =
    [ Predicate isDOMOrdinal
    ]
  , prod = \case
      (Token Ordinal OrdinalData{TOrdinal.value = v}:_) ->
        tt $ dayOfMonth v
      _ -> Nothing
  }

ruleOrdinalCycleOfTime :: Rule
ruleOrdinalCycleOfTime = Rule
  { name = "<ordinal> <cycle> of <time>"
  , pattern =
    [ dimension Ordinal
    , dimension TimeGrain
    , regex "в"
    , dimension Time
    ]
  , prod = \case
      (Token Ordinal od:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter True grain (TOrdinal.value od - 1) td
      _ -> Nothing
  }


ruleHhmm :: Rule
ruleHhmm = Rule
  { name = "hh:mm"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3]))[:ч]([0-5]\\d)(?:час(ов|а|у)?|ч)?"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        h <- parseInt m1
        m <- parseInt m2
        tt $ hourMinute False h m
      _ -> Nothing
  }

ruleYear :: Rule
ruleYear = Rule
  { name = "year"
  , pattern =
    [ Predicate $ isIntegerBetween 1000 2100
    ]
  , prod = \case
      (token:_) -> do
        y <- getIntValue token
        tt $ year y
      _ -> Nothing
  }

ruleNamedmonthDayofmonthNonOrdinal :: Rule
ruleNamedmonthDayofmonthNonOrdinal = Rule
  { name = "<named-month> <day-of-month> (non ordinal)"
  , pattern =
    [ Predicate isAMonth
    , Predicate isDOMInteger
    ]
  , prod = \case
      (Token Time td:token:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleHhmmMilitary :: Rule
ruleHhmmMilitary = Rule
  { name = "hhmm (military)"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3]))([0-5]\\d)"
    ]
  , prod = \case
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
  , prod = \case
      (x:_) -> Just x
      _ -> Nothing
  }

ruleLastDayofweekOfTime :: Rule
ruleLastDayofweekOfTime = Rule
  { name = "last <day-of-week> of <time>"
  , pattern =
    [ regex "последний"
    , Predicate isADayOfWeek
    , regex "в"
    , dimension Time
    ]
  , prod = \case
      (_:Token Time td1:_:Token Time td2:_) ->
        tt $ predLastOf td1 td2
      _ -> Nothing
  }

ruleTimeofdayTimeofdayInterval :: Rule
ruleTimeofdayTimeofdayInterval = Rule
  { name = "<time-of-day> - <time-of-day> (interval)"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isATimeOfDay]
    , regex "\\-"
    , Predicate isATimeOfDay
    ]
  , prod = \case
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleTimeofdayTimeofdayInterval2 :: Rule
ruleTimeofdayTimeofdayInterval2 = Rule
  { name = "<time-of-day> - <time-of-day> (interval)"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "\\-|/"
    , Predicate $ and . sequence [isNotLatent, isATimeOfDay]
    ]
  , prod = \case
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleDurationAfterTime :: Rule
ruleDurationAfterTime = Rule
  { name = "<duration> after <time>"
  , pattern =
    [ dimension Duration
    , regex "после"
    , dimension Time
    ]
  , prod = \case
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
  , prod = \case
      (Token Ordinal OrdinalData{TOrdinal.value = v}:_) -> tt .
        cycleNthAfter False TG.Quarter (v - 1) $ cycleNth TG.Year 0
      _ -> Nothing
  }

ruleDurationBeforeTime :: Rule
ruleDurationBeforeTime = Rule
  { name = "<duration> before <time>"
  , pattern =
    [ dimension Duration
    , regex "перед"
    , dimension Time
    ]
  , prod = \case
      (Token Duration dd:_:Token Time td:_) ->
        tt $ durationBefore dd td
      _ -> Nothing
  }

rulePartofdayOfTime :: Rule
rulePartofdayOfTime = Rule
  { name = "<part-of-day> of <time>"
  , pattern =
    [ Predicate isAPartOfDay
    , dimension Time
    ]
  , prod = \case
      (Token Time td1:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleMmddyyyy :: Rule
ruleMmddyyyy = Rule
  { name = "mm/dd/yyyy"
  , pattern =
    [ regex "([012]?[1-9]|10|20|30|31)\\.(0?[1-9]|10|11|12)\\.(\\d{2,4})"
    ]
  , prod = \case
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
    , regex "час(у|а|ов)?|ч(?:[\\s'\"-_{}\\[\\]()]|$)"
    ]
  , prod = \case
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
  , prod = \case
      (token:
       Token Time td:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> do
        n <- parseInt match
        dom <- intersectDOM td token
        Token Time <$> intersect dom (year n)
      _ -> Nothing
  }

ruleTimezone :: Rule
ruleTimezone = Rule
  { name = "<time> timezone"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isATimeOfDay]
    , regex "\\b(YEKT|YEKST|YAKT|YAKST|WITA|WIT|WIB|WGT|WGST|WFT|WET|WEST|WAT|WAST|VUT|VLAT|VLAST|VET|UZT|UYT|UYST|UTC|ULAT|TVT|TMT|TLT|TKT|TJT|TFT|TAHT|SST|SRT|SGT|SCT|SBT|SAST|SAMT|RET|PYT|PYST|PWT|PST|PONT|PMST|PMDT|PKT|PHT|PHOT|PGT|PETT|PETST|PET|PDT|OMST|OMSST|NZST|NZDT|NUT|NST|NPT|NOVT|NOVST|NFT|NDT|NCT|MYT|MVT|MUT|MST|MSK|MSD|MMT|MHT|MDT|MAWT|MART|MAGT|MAGST|LINT|LHST|LHDT|KUYT|KST|KRAT|KRAST|KGT|JST|IST|IRST|IRKT|IRKST|IRDT|IOT|IDT|ICT|HOVT|HKT|GYT|GST|GMT|GILT|GFT|GET|GAMT|GALT|FNT|FKT|FKST|FJT|FJST|EST|EGT|EGST|EET|EEST|EDT|ECT|EAT|EAST|EASST|DAVT|ChST|CXT|CVT|CST|COT|CLT|CLST|CKT|CHAST|CHADT|CET|CEST|CDT|CCT|CAT|CAST|BTT|BST|BRT|BRST|BOT|BNT|AZT|AZST|AZOT|AZOST|AWST|AWDT|AST|ART|AQTT|ANAT|ANAST|AMT|AMST|ALMT|AKST|AKDT|AFT|AEST|AEDT|ADT|ACST|ACDT)\\b"
    ]
  , prod = \case
      (Token Time td:
       Token RegexMatch (GroupMatch (tz:_)):
       _) -> Token Time <$> inTimezone (Text.toUpper tz) td
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ruleAbsorptionOfAfterNamedDay
  , ruleAfterTimeofday
  , ruleAfternoon
  , ruleAtTimeofday
  , ruleBetweenTimeofdayAndTimeofdayInterval
  , ruleDatetimeDatetimeInterval
  , ruleDateDateInterval
  , ruleDayofmonthNonOrdinalNamedmonth
  , ruleDayofmonthOrdinal
  , ruleDayofmonthordinalNamedmonth
  , ruleDayofmonthordinalNamedmonthYear
  , ruleDurationAfterTime
  , ruleDurationAgo
  , ruleDurationBeforeTime
  , ruleEvening
  , ruleHhmm
  , ruleHhmmMilitary
  , ruleHourofdayIntegerAsRelativeMinutes
  , ruleInDuration
  , ruleIntersect
  , ruleIntersectBy
  , ruleIntersectByOfFromS
  , ruleLastCycle
  , ruleBeforeLastCycle
  , ruleLastCycle2
  , ruleLastCycleOfTime
  , ruleLastCycleOfTime2
  , ruleLastDayofweekOfTime
  , ruleLastNCycle
  , ruleLastTime
  , ruleLunch
  , ruleMidnighteodendOfDay
  , ruleMmdd
  , ruleMmddyyyy
  , ruleMonthDdddInterval
  , ruleMorning
  , ruleNamedmonthDayofmonthNonOrdinal
  , ruleNamedmonthDayofmonthOrdinal
  , ruleNextCycle
  , ruleNextNCycle
  , ruleNight
  , ruleNoon
  , ruleMidnight
  , ruleNthTimeAfterTime
  , ruleNthTimeOfTime
  , ruleNthTimeOfTime2
  , ruleOnDate
  , ruleOrdinalCycleOfTime
  , ruleOrdinalQuarter
  , ruleOrdinalQuarterYear
  , rulePartofdayOfTime
  , ruleThisCycle
  , ruleThisPartofday
  , ruleThisTime
  , ruleThisnextDayofweek
  , ruleTimeAfterNext
  , ruleTimePartofday
  , ruleTimeofdayLatent
  , ruleTimeofdayOclock
  , ruleTimeofdayTimeofdayInterval
  , ruleTimeofdayTimeofdayInterval2
  , ruleUntilTimeofday
  , ruleWeek
  , ruleWeekend
  , ruleWithinDuration
  , ruleYear
  , ruleYearLatent
  , ruleYearLatent2
  , ruleYyyymmdd
  , ruleTimezone
  ]
  ++ ruleInstants
  ++ ruleDaysOfWeek
  ++ ruleMonths
  ++ ruleSeasons
  ++ ruleHolidays
