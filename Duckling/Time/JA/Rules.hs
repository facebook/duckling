-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.JA.Rules
  ( rules
  ) where

import Control.Monad (guard)
import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Duration.Types (DurationData (..))
import Duckling.Numeral.Helpers (isNatural, parseInt)
import Duckling.Regex.Types
import Duckling.Time.Helpers
import Duckling.Types
import qualified Duckling.Duration.Types as TDuration
import qualified Duckling.Time.Types as TTime
import qualified Duckling.TimeGrain.Types as TG


ruleHHMM :: Rule
ruleHHMM = Rule
  { name = "hh:mm"
  , pattern = [regex "((?:[01]?\\d)|(?:2[0-3]))[：:. ]([0-5]\\d)"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (hh:mm:_)):_) -> do
        h <- parseInt hh
        m <- parseInt mm
        tt $ hourMinute (h /= 0 && h < 12) h m
      _ -> Nothing
  }

ruleHHMMKanji :: Rule
ruleHHMMKanji = Rule
  { name = "hh時mm分"
  , pattern = [regex "((?:[01]?\\d)|(?:2[0-3]))時([0-5]\\d)分"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (hh:mm:_)):_) -> do
        h <- parseInt hh
        m <- parseInt mm
        tt $ hourMinute (h /= 0 && h < 12) h m
      _ -> Nothing
  }

ruleHHMMKanjiNumeral :: Rule
ruleHHMMKanjiNumeral = Rule
  { name = "hh時mm分"
  , pattern =
    [ Predicate isNatural
    , regex "時"
    , Predicate isNatural
    , regex "分"
    ]
  , prod = \tokens -> case tokens of
      (hh:_:mm:_) -> do
          h <- getIntValue hh
          m <- getIntValue mm
          guard (h > 0 && h < 25 && m < 60)
          tt $ hourMinute (h /= 0 && h < 12) h m
      _ -> Nothing
  }

ruleTODLatent :: Rule
ruleTODLatent = Rule
  { name = "time-of-day (latent)"
  , pattern =
    [ Predicate $ isIntegerBetween 0 23
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt $ mkLatent $ hour (n < 13) n
      _ -> Nothing
  }

ruleHHOClock :: Rule
ruleHHOClock = Rule
  { name = "<time-of-day> o'clock"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "時(ちょうど|きっかり|ぴったり)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleTODAMPM :: Rule
ruleTODAMPM = Rule
  { name = "<time-of-day> am|pm"
  , pattern =
      [ Predicate isATimeOfDay
      , regex "([ap])(\\s|\\.)?m?\\.?"
      ]
    , prod = \tokens -> case tokens of
        (Token Time td:Token RegexMatch (GroupMatch (ap:_)):_) ->
          tt $ timeOfDayAMPM (Text.toLower ap == "a") td
        _ -> Nothing
    }

ruleTODAMPMKanji :: Rule
ruleTODAMPMKanji = Rule
  { name = "午後|午前 <time-of-day>"
  , pattern =
    [ regex "午(前|後)"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (ap:_)):Token Time td:_) ->
        tt $ timeOfDayAMPM (ap == "前") td
      _ -> Nothing
  }

ruleAtTOD :: Rule
ruleAtTOD = Rule
  { name = "at <time-of-day>"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "に|で|の"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleMMDD :: Rule
ruleMMDD = Rule
  { name = "mm/dd"
  , pattern =
    [ regex "(1[0-2]|0?[1-9])\\s?[/／]\\s?(3[01]|[12]\\d|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (mm:dd:_)):_) -> do
        m <- parseInt mm
        d <- parseInt dd
        tt $ monthDay m d
      _ -> Nothing
  }

ruleYYYYMM :: Rule
ruleYYYYMM = Rule
  { name = "yyyy/mm"
  , pattern =
    [ regex "(\\d{4})\\s*[/／.]\\s*(1[0-2]|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (yy:mm:_)):_) -> do
        y <- parseInt yy
        m <- parseInt mm
        tt $ yearMonth y m
      _ -> Nothing
  }

ruleYYYYMMDD :: Rule
ruleYYYYMMDD = Rule
  { name = "yyyy/mm/dd"
  , pattern =
    [ regex "(\\d{2,4})\\s*[/／.]\\s*(0?[1-9]|1[0-2])\\s*[/／.]\\s*(3[01]|[12]\\d|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (yy:mm:dd:_)):_) -> do
        y <- parseInt yy
        m <- parseInt mm
        d <- parseInt dd
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

ruleMonthDOM :: Rule
ruleMonthDOM = Rule
  { name = "<named-month> <day-of-month>"
  , pattern =
    [ Predicate isAMonth
    , Predicate isDOMValue
    , regex "日"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:token:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleYearMonthDOM :: Rule
ruleYearMonthDOM = Rule
  { name = "<year> <named-month> <day-of-month>"
  , pattern =
    [ regex "(\\d{2,4})年"
    , Predicate isAMonth
    , Predicate isDOMValue
    , regex "日"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):Token Time td:token:_) -> do
        intVal <- parseInt match
        dom <- intersectDOM td token
        Token Time <$> intersect dom (year intVal)
      _ -> Nothing
  }

ruleImperialYearMonthDOM :: Rule
ruleImperialYearMonthDOM = Rule
  { name = "imperial <year> <named-month> <day-of-month>"
  , pattern =
    [ Predicate $ isGrainOfTime TG.Year
    , Predicate isAMonth
    , Predicate isDOMValue
    , regex "日"
    ]
  , prod = \tokens -> case tokens of
      (Token Time year:Token Time td:token:_) -> do
        dom <- intersectDOM td token
        Token Time <$> intersect dom year
      _ -> Nothing
  }

ruleYearMonth :: Rule
ruleYearMonth = Rule
  { name = "<year> <named-month>"
  , pattern =
    [ Predicate $ isGrainOfTime TG.Year
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (Token Time year:Token Time month:_) ->
        Token Time . notLatent <$> intersect year month
      _ -> Nothing
  }

ruleOnADOW :: Rule
ruleOnADOW = Rule
  { name = "on <named-day>"
  , pattern =
    [ regex "(\\(|（)?"
    , Predicate isADayOfWeek
    , regex "(\\)|）)?(に|は|で)"
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleAbsorbOnDay :: Rule
ruleAbsorbOnDay = Rule
  { name = "on <day>"
  , pattern =
    [ Predicate $ isGrainOfTime TG.Day
    , regex "に|は|で"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleOnDay :: Rule
ruleOnDay = Rule
  { name = "on the <day-of-month>"
  , pattern =
    [ Predicate isDOMValue
    , regex "日(に|は|で)"
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt $ dayOfMonth n
      _ -> Nothing
  }

ruleInNamedMonth :: Rule
ruleInNamedMonth = Rule
  { name = "in <named-month>"
  , pattern =
    [ Predicate isAMonth
    , regex "(の間|中)?(に|は|で)"
    ]
  , prod = \tokens -> case tokens of
      (td2:_) -> Just td2
      _ -> Nothing
  }

ruleYearLatent :: Rule
ruleYearLatent = Rule
  { name = "year (latent)"
  , pattern =
      [ Predicate $ or . sequence [isIntegerBetween (- 10000) 0, isIntegerBetween 25 10000]
      , regex "年"
      ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt $ mkLatent $ year n
      _ -> Nothing
  }

ruleImperialYearLatent :: Rule
ruleImperialYearLatent = Rule
  { name = "Imperial year (latent)"
  , pattern =
      [ regex "(明治|大正|昭和|平成|令和)"
      , Predicate $ isIntegerBetween 0 65
      , regex "年"
      ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):token:_) -> do
        n <- getIntValue token
        ry <- case Text.toLower match of
            "明治" -> Just 1868
            "大正" -> Just 1912
            "昭和" -> Just 1926
            "平成" -> Just 1989
            "令和" -> Just 2019
            _     -> Nothing
        tt $ mkLatent $ year (ry+n-1)
      _ -> Nothing
  }

ruleFirstImperialYearLatent :: Rule
ruleFirstImperialYearLatent = Rule
  { name = "First Imperial year (latent)"
  , pattern =
      [ regex "(明治|大正|昭和|平成|令和)元年"
      ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        ry <- case Text.toLower match of
            "明治" -> Just 1868
            "大正" -> Just 1912
            "昭和" -> Just 1926
            "平成" -> Just 1989
            "令和" -> Just 2019
            _     -> Nothing
        tt $ mkLatent $ year ry
      _ -> Nothing
  }

ruleInYear :: Rule
ruleInYear = Rule
  { name = "in <year>"
  , pattern =
    [ Predicate $ isGrainOfTime TG.Year
    , regex "に|は|で"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleInMonth :: Rule
ruleInMonth = Rule
  { name = "in <month>"
  , pattern =
    [ Predicate $ isGrainOfTime TG.Month
    , regex "に"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleInDuration :: Rule
ruleInDuration = Rule
  { name = "in <duration>"
  , pattern =
    [ dimension Duration
    , regex "後|(間)?で|経ったら|経ってから|経過後に|経過してから|経過したら|過ぎに|過ぎたら"
    ]
  , prod = \tokens -> case tokens of
        (Token Duration dd:_) ->
          tt $ inDuration dd
        _ -> Nothing
  }

ruleInTheLastDuration :: Rule
ruleInTheLastDuration = Rule
  { name = "in/for the last <duration>"
  , pattern =
    [ regex "この"
    , Predicate $ isDurationGreaterThan TG.Hour
    ]
  , prod = \tokens -> case tokens of
        (_:Token Duration dd:_) ->
          Token Time <$> interval TTime.Closed (durationBefore dd now) now
        _ -> Nothing
  }

ruleIntervalFromTime :: Rule
ruleIntervalFromTime = Rule
  { name = "from <time>"
  , pattern =
    [ dimension Time
    , regex "(初め)?以(降|来)に?|～|よりも?後|から"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ withDirection TTime.After $ notLatent td
      _ -> Nothing
  }

ruleIntervalUntilTime :: Rule
ruleIntervalUntilTime = Rule
  { name = "until <time>"
  , pattern =
    [ dimension Time
    , regex "までに?|よりも?前|以前"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ withDirection TTime.Before $ notLatent td
      _ -> Nothing
  }

ruleIntervalFromToTime :: Rule
ruleIntervalFromToTime = Rule
  { name = "from <time> to <time>"
  , pattern =
    [ dimension Time
    , regex "(初め)?以(降|来)に?|～|よりも?後?|(初め|頭)?から|以降"
    , dimension Time
    , regex "末?まで|(より|以)前|の間|にかけて|いっぱい"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleIntervalTimes :: Rule
ruleIntervalTimes = Rule
  { name = "<time> - <time>"
  , pattern =
    [ dimension Time
    , regex "-|~|～|・|、|から"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleIntervalYear :: Rule
ruleIntervalYear = Rule
  { name = "<year> - <year>"
  , pattern =
    [ Predicate $ isIntegerBetween 1000 10000
    , regex "-|~|～|・|、|から"
    , Predicate $ isIntegerBetween 1000 10000
    , regex "年"
    ]
  , prod = \tokens -> case tokens of
      (t1:_:t2:_) -> do
        y1 <- getIntValue t1
        y2 <- getIntValue t2
        guard (y1 < y2)
        Token Time <$> interval TTime.Closed (year y1) (year y2)
      _ -> Nothing
  }

ruleWeekend :: Rule
ruleWeekend = Rule
  { name = "week-end"
  , pattern =
    [ regex "週末|しゅうまつ"
    ]
  , prod = \_ -> tt $ mkOkForThisNext weekend
  }

ruleNow :: Rule
ruleNow = Rule
  { name = "now"
  , pattern =
    [ regex "今すぐ|いま|即|ただ(いま|ちに)"
    ]
  , prod = \_ -> tt now
  }

ruleThisTimeGrain :: Rule
ruleThisTimeGrain = Rule
  { name = "this|current <time-grain>"
  , pattern =
    [ regex "現在の|(今|こ(ん|の)|当|現)(1|一)?"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      ( _:
        Token TimeGrain grain:
        _) -> tt $ cycleNth grain 0
      _ -> Nothing
  }

ruleNextTimeGrain :: Rule
ruleNextTimeGrain = Rule
  { name = "next <time-grain>"
  , pattern =
    [ regex "来"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      ( _:
        Token TimeGrain grain:
        _) -> tt $ cycleNth grain 1
      _ -> Nothing
  }

ruleLastTimeGrain :: Rule
ruleLastTimeGrain = Rule
  { name = "last <time-grain>"
  , pattern =
    [ regex "(前|直近|最後|昨|先)の?(1|一)?"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      ( _:
        Token TimeGrain grain:
        _) -> tt $ cycleNth grain $ - 1
      _ -> Nothing
  }

ruleDurationLastNext :: Rule
ruleDurationLastNext = Rule
  { name = "last|past|next <duration>"
  , pattern =
    [ regex "(過去|直在|次の)"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       Token Duration DurationData{TDuration.grain, TDuration.value}:
       _) -> case Text.toLower match of
         "次の" -> tt $ cycleN True grain value
         "過去" -> tt $ cycleN True grain (- value)
         "直在" -> tt $ cycleN True grain (- value)
         _      -> Nothing
      _ -> Nothing
  }

ruleInstants :: [Rule]
ruleInstants = mkRuleInstants
  [ ("today"            , TG.Day    , 0   , "今日(1日)?|きょう|本日|ほんじつ"       )
  , ("tomorrow"         , TG.Day    , 1   , "明日|あした|あす|みょうにち"           )
  , ("yesterday"        , TG.Day    , -1  , "(前|昨)日|きのう|さくじつ"            )
  , ("next week"        , TG.Week   , 1   , "らいしゅう"                         )
  , ("last week"        , TG.Week   , -1  , "せんしゅう"                         )
  , ("a week after next", TG.Week   , 2   , "再来週|さらいしゅう"                 )
  , ("this month"       , TG.Month  , 0   , "このひと月"                         )
  , ("this quarter"     , TG.Quarter, 0   , "本四半期|今の四半期|現行四半期"        )
  , ("this year"        , TG.Year   , 0   , "本年"                              )
  , ("last year"        , TG.Year   , -1  , "去年"                              )
  ]

ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = mkRuleDaysOfWeek
  [ ( "Monday"   , "月曜日?|げつようび"    )
  , ( "Tuesday"  , "火曜日?|かようび"      )
  , ( "Wednesday", "水曜日?|すいようび"    )
  , ( "Thursday" , "木曜日?|もくようび"    )
  , ( "Friday"   , "金曜日?|きんようび"    )
  , ( "Saturday" , "土曜日?|どようび"      )
  , ( "Sunday"   , "日曜日?|にちようび"     )
  ]

ruleDaysOfWeekLatent :: [Rule]
ruleDaysOfWeekLatent = mkRuleDaysOfWeekLatent
  [ ( "Monday"   , "月|げつよう"    )
  , ( "Tuesday"  , "火|かよう"      )
  , ( "Wednesday", "水|すいよう"    )
  , ( "Thursday" , "木|もくよう"    )
  , ( "Friday"   , "金|きんよう"    )
  , ( "Saturday" , "土|どよう"      )
  , ( "Sunday"   , "日|にちよう"     )
  ]

ruleMonths :: [Rule]
ruleMonths = mkRuleMonths
  [ ( "January"  , "(1|一|１)(月|がつ)|いちがつ"            )
  , ( "February" , "(2|二|２)(月|がつ)|にがつ"              )
  , ( "March"    , "(3|三|３)(月|がつ)|さんがつ"            )
  , ( "April"    , "(4|四|４)(月|がつ)|しがつ"              )
  , ( "May"      , "(5|五|５)(月|がつ)|ごがつ"              )
  , ( "June"     , "(6|六|６)(月|がつ)|ろくがつ"            )
  , ( "July"     , "(7|七|７)(月|がつ)|しちがつ"            )
  , ( "August"   , "(8|八|８)(月|がつ)|はちがつ"            )
  , ( "September", "(9|九|９)(月|がつ)|くがつ"              )
  , ( "October"  , "(10|十|１０)(月|がつ)|じゅうがつ"        )
  , ( "November" , "(11|十一|１１)(月|がつ)|じゅういちがつ"   )
  , ( "December" , "(12|十二|１２)(月|がつ)|じゅうにがつ"     )
  ]

rules :: [Rule]
rules =
  [ ruleHHMM
  , ruleHHMMKanji
  , ruleHHMMKanjiNumeral
  , ruleTODLatent
  , ruleHHOClock
  , ruleTODAMPM
  , ruleTODAMPMKanji
  , ruleAtTOD
  , ruleMMDD
  , ruleYYYYMM
  , ruleYYYYMMDD
  , ruleYearLatent
  , ruleImperialYearLatent
  , ruleFirstImperialYearLatent
  , ruleMonthDOM
  , ruleYearMonth
  , ruleYearMonthDOM
  , ruleImperialYearMonthDOM
  , ruleOnADOW
  , ruleAbsorbOnDay
  , ruleOnDay
  , ruleInNamedMonth
  , ruleInYear
  , ruleInMonth
  , ruleInDuration
  , ruleInTheLastDuration
  , ruleIntervalFromTime
  , ruleIntervalUntilTime
  , ruleIntervalFromToTime
  , ruleIntervalTimes
  , ruleIntervalYear
  , ruleWeekend
  , ruleNow
  , ruleThisTimeGrain
  , ruleNextTimeGrain
  , ruleLastTimeGrain
  , ruleDurationLastNext
  ]
  ++ ruleDaysOfWeek
  ++ ruleDaysOfWeekLatent
  ++ ruleMonths
  ++ ruleInstants
