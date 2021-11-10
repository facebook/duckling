-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.TR.Rules
  ( rules
  ) where

import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers (isGrain)
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Types (OrdinalData(..))
import Duckling.Regex.Types (GroupMatch(..))
import Duckling.Time.Computed
import Duckling.Time.Helpers
import Duckling.Time.Types (TimeData(..), TimeIntervalType (Closed, Open))
import Duckling.Types
import qualified Duckling.Ordinal.Types as TOrdinal
import qualified Duckling.Time.Types as TTime
import qualified Duckling.TimeGrain.Types as TG

ruleInstants :: [Rule]
ruleInstants = mkRuleInstants
  [ ( "now"             , TG.Second,  0, "ş?imdi|şu(\\s)?an"                                )
  , ( "today"           , TG.Day   ,  0, "bugün"                                                         )
  , ( "tomorrow"        , TG.Day   ,  1, "yarın"                                                         )
  , ( "yesterday"       , TG.Day   , -1, "dün"                                                           )
  , ( "after tomorrow"  , TG.Day   ,  2, "(yarından\\ssonraki)\\s?(gün)?"                                 )
  , ( "before yesterday", TG.Day   , -2, "(dün\\sdeğil\\sevvelsi|dünden\\sönceki|öbürsü|öbürki)\\s(gün)?")
  , ( "EOM|End of day",   TG.Day ,    1, "gün\\s(sonu(na|ndan?)?|bitimi(ne|nden?)?)?"                    )
  , ( "EOM|End of month", TG.Month ,  1, "ay\\s(sonu(na|ndan?)?|bitimi(ne|nden?)?)?"                     )
  , ( "EOY|End of year" , TG.Year  ,  1, "yıl\\s(sonu(na|ndan?)?|bitimi(ne|nden?)?)?"                    )
  ]

ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = mkRuleDaysOfWeek
  [ ( "Pazartesi", "pazartesi'?(si|den|ye)?|pzts?")
  , ( "Salı"     , "salı?'?(sı|dan|ya)?"          )
  , ( "Çarşamba" , "çar(şamba)?'?(sı|dan|ya)?"    )
  , ( "Perşembe" , "per(şembe)?'?(si|den|ye)?"    )
  , ( "Cuma"     , "cuma?'?(sı|dan|ya)?"          )
  , ( "Cumartesi", "cumartesi'?(si|den|ye)?|cmt"  )
  , ( "Pazar"    , "paz(ar)?'?(ı|dan|a)?"         )
  ]

ruleMonths :: [Rule]
ruleMonths = mkRuleMonths
  [ ( "Ocak"   , "ocak?'?(ğın|tan|ğ?a)?"    )
  , ( "Şubat"  , "şub(at)?'?(a|ın|tan)?"    )
  , ( "Mart"   , "mart?'?(ın|a|tan)?"       )
  , ( "Nisan"  , "nis(an)?'?(ın|a|dan)?"    )
  , ( "Mayıs"  , "may(ıs)?'?(ın|a|tan)?"    )
  , ( "Haziran", "haz(iran)?'?(ın|a|dan)?"  )
  , ( "Temmuz" , "tem(muz)?'?(un|a|dan)?"   )
  , ( "Ağustos", "ağu(stos)?'?(un|a|tan)?"  )
  , ( "Eylül"  , "eyl(ül)?'?(ün|e|den)?"    )
  , ( "Ekim"   , "ekim?'?(in|den|e)?"       )
  , ( "Kasım"  , "kas(ım)?'?(ın|dan|a)?"    )
  , ( "Aralık" , "ara(lık?)?'?(ğın|ğa|tan)?")
  ]

ruleSeasons :: [Rule]
ruleSeasons = mkRuleSeasons
  [ ( "yaz",      "yaz(ın|a|dan)?",      monthDay 6  21, monthDay 9  23 )
  , ( "sonbahar", "sonbahar(ın|a|dan)?", monthDay 9  23, monthDay 12 21 )
  , ( "kış",      "kış(ın|a|tan)?",      monthDay 12 21, monthDay 3  20 )
  , ( "ilkbahar", "ilkbahar(ın|a|dan)?", monthDay 3  20, monthDay 6  21 )
  ]

ruleHolidays :: [Rule]
ruleHolidays = mkRuleHolidays
  [ ( "Yılbaşı"                                , "yılbaşı(ndan|na)?|yılbaşı(\\statili(nden|ne))?"                   , monthDay 1   1  )
  , ( "Ulusal Egemenlik ve Çocuk Bayramı"      , "(ulusal\\segemenlik\\sve\\s)?çocuk\\sbayramı"                     , monthDay 4   23 )
  , ( "Emek ve Dayanışma Günü"                 , "emek\\sve\\sdayanışma\\sgünü"                                     , monthDay 5   1  )
  , ( "Atatürk’ü Anma, Gençlik ve Spor Bayramı", "(gençlik\\sve\\sspor|spor|gençlik)\\sbayramı"                     , monthDay 5   19 )
  , ( "Zafer Bayramı"                          , "zafer\\sbayramı"                                                  , monthDay 8   30 )
  , ( "Cumhuriyet Bayramı"                     , "cumhuriyet\\sbayramı"                                             , monthDay 10  29 )
  ]

ruleComputedHolidays:: [Rule]
ruleComputedHolidays = mkRuleHolidays
  [ ("Kurban Bayramı",  "kurban\\sbayramı",  eidalAdha)
  , ("Ramazan Bayramı", "ramazan\\sbayramı", eidalFitr)
  ]

rulePartOfDays :: Rule
rulePartOfDays = Rule
  { name = "part of days"
  , pattern =
    [ regex "(sabah(ı|a|tan)?|öğlen?|akşam|gece|öğle\\syemeği)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        let (start, end) = case Text.toLower match of
              "sabah"          -> (hour False 0, hour False 12 )
              "sabahı"         -> (hour False 0, hour False 12 )
              "sabaha"         -> (hour False 0, hour False 12 )
              "sabahtan"       -> (hour False 0, hour False 12 )
              "akşam"          -> (hour False 18, hour False 0 )
              "gece"           -> (hour False 18, hour False 0 )
              "öğlen"          -> (hour False 12, hour False 14)
              "öğle"           -> (hour False 12, hour False 14)
              "öğle yemeği"    -> (hour False 12, hour False 14)
              _                -> (hour False 12, hour False 19)
        td <- interval TTime.Open start end
        tt $ partOfDay $ mkLatent td
      _ -> Nothing
  }

rulePrecisionTOD :: Rule
rulePrecisionTOD = Rule
  { name = "about|exactly <time-of-day>"
  , pattern =
    [ Predicate $ isGrainFinerThan TG.Year
    , regex "gibi|civarı(nda)?"
    ]
  , prod = \case
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

rulePrecisionTOD2 :: Rule
rulePrecisionTOD2 = Rule
  { name = "about|exactly <time-of-day>"
  , pattern =
    [ regex "yaklaşık|tam(\\solarak)?"
    , Predicate $ isGrainFinerThan TG.Year
    ]
  , prod = \case
      (_:Token Time td:_) -> tt $ notLatent td
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
    , regex "\\-|/"
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
    [ dimension Duration
    , regex "içinde|içerisinde"
    ]
  , prod = \case
      (Token Duration dd:_) ->
        tt $ inDuration dd
      _ -> Nothing
  }

ruleDurationHence :: Rule
ruleDurationHence = Rule
  { name = "<duration> hence"
  , pattern =
    [ dimension Duration
    , regex "sonra"
    ]
  , prod = \case
      (Token Duration dd:_) ->
        tt $ inDuration dd
      _ -> Nothing
  }

ruleDurationFromNow :: Rule
ruleDurationFromNow = Rule
  { name = "<duration> from now"
  , pattern =
    [ regex "(bugünden\\s)?sonra(ki)?"
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
    [ dimension Time
    , regex "son"
    , dimension TimeGrain
    ]
  , prod = \case
      (Token Time td:_:Token TimeGrain grain:_) ->
        tt $ cycleLastOf grain td
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
        tt $ mkLatent $ year v
      _ -> Nothing
  }

ruleNoon :: Rule
ruleNoon = Rule
  { name = "noon"
  , pattern =
    [ regex "öğle(n|den|ye)?"
    ]
  , prod = \_ -> tt $ hour False 12
  }

ruleThisnextDayofweek :: Rule
ruleThisnextDayofweek = Rule
  { name = "this|next <day-of-week>"
  , pattern =
    [ regex "bu|sonraki"
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
    [ Predicate isATimeOfDay
    , regex "ile"
    , Predicate isATimeOfDay
    , regex "arası"
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
    [ regex "sonraki|önümüzdeki|gelecek"
    , dimension TimeGrain
    ]
  , prod = \case
      (_:Token TimeGrain grain:_) ->
        tt $ cycleNth grain 1
      _ -> Nothing
  }

ruleLastCycle :: Rule
ruleLastCycle = Rule
  { name = "last <cycle>"
  , pattern =
    [ regex "son|geçen|geçtiğimiz|önceki"
    , dimension TimeGrain
    ]
  , prod = \case
      (_:Token TimeGrain grain:_) ->
        tt $ cycleNth grain (-1)
      _ -> Nothing
  }

ruleLunch :: Rule
ruleLunch = Rule
  { name = "lunch"
  , pattern =
    [ regex "öğlen? (yemeği|arası)"
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
    [ regex "öğleden sonra"
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
    [ regex "akşam(a|dan)?"
    ]
  , prod = \_ ->
      let from = hour False 18
          to = hour False 0
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
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

ruleIntervalBy :: Rule
ruleIntervalBy = Rule
  { name = "by <time>"
  , pattern =
    [ dimension Time
    ,  regex "kadar"
    ]
  , prod = \case
      (Token Time td:_) -> Token Time <$> interval TTime.Open now td
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
    [ dimension Time
    , regex "sonra(ki)?"
    , dimension Ordinal
    , dimension Time
    ]
  , prod = \case
      (Token Time td1:
       _:
       Token Ordinal OrdinalData{TOrdinal.value = v}:
       Token Time td2:
       _) -> tt $ predNthAfter (v - 1) td1 td2
      _ -> Nothing
  }

ruleMMDD :: Rule
ruleMMDD = Rule
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

ruleMMDDYYYY :: Rule
ruleMMDDYYYY = Rule
  { name = "mm/dd/yyyy"
  , pattern =
    [ regex "(1[0-2]|0?[1-9])[-/\\s](3[01]|[12]\\d|0?[1-9])[-/\\s](\\d{2,4})"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (mm:dd:yy:_)):_) -> do
        y <- parseInt yy
        m <- parseInt mm
        d <- parseInt dd
        tt $ yearMonthDay y m d
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
        tt $ mkLatent $ hour (n < 12) n
      _ -> Nothing
  }

ruleDurationAgo :: Rule
ruleDurationAgo = Rule
  { name = "<duration> ago"
  , pattern =
    [ dimension Duration
    , regex "önce"
    ]
  , prod = \case
      (Token Duration dd:_:_) ->
        tt $ durationAgo dd
      _ -> Nothing
  }

ruleIdesOfMonth :: Rule
ruleIdesOfMonth = Rule
  { name = "the ides of <named-month>"
  , pattern =
    [ Predicate isAMonth
    , regex "ortası"
    ]
  , prod = \case
      (Token Time td@TimeData {TTime.form = Just (TTime.Month m)}:_) ->
        Token Time <$>
          intersect td (dayOfMonth $ if elem m [3, 5, 7, 10] then 15 else 13)
      _ -> Nothing
  }

ruleLastNCycle :: Rule
ruleLastNCycle = Rule
  { name = "last n <cycle>"
  , pattern =
    [ regex "son|geçen|geçtiğimiz|önceki"
    , Predicate $ isIntegerBetween 1 9999
    , dimension TimeGrain
    ]
  , prod = \case
      (_:token:Token TimeGrain grain:_) -> do
        n <- getIntValue token
        tt $ cycleN True grain (- n)
      _ -> Nothing
  }

ruleMidnighteodendOfDay :: Rule
ruleMidnighteodendOfDay = Rule
  { name = "midnight|EOD|end of day"
  , pattern =
    [ regex "gece\\syarısı|gün\\ssonu|gün\\sbitimi"
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
    [ dimension Time
    , regex "kadar"
    ]
  , prod = \case
      (_:Token Time td:_) -> tt $ withDirection TTime.Before td
      _ -> Nothing
  }

ruleAtTimeofday :: Rule
ruleAtTimeofday = Rule
  { name = "at <time-of-day>"
  , pattern =
    [ regex "saat"
    , Predicate isATimeOfDay
    ]
  , prod = \case
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleAtTimeofday2 :: Rule
ruleAtTimeofday2 = Rule
  { name = "at <time-of-day>"
  , pattern =
    [ regex "saat"
    , Predicate isATimeOfDay
    , regex "'?(den|dan)?"
    ]
  , prod = \case
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleNthTimeOfTime :: Rule
ruleNthTimeOfTime = Rule
  { name = "nth <time> of <time>"
  , pattern =
    [ dimension Time
    , dimension Ordinal
    , dimension Time
    ]
  , prod = \case
      (Token Time td1:
       Token Ordinal OrdinalData{TOrdinal.value = v}:
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
   [ regex "((bu\\s)?hafta boyunca)|(bu hafta)|(haftanın geri kalanı)"
   ]
 , prod = \case
     (Token RegexMatch (GroupMatch (match:_)):_) ->
       let end = cycleNthAfter True TG.Day (-2) $ cycleNth TG.Week 1
           period = case Text.toLower match of
                      "hafta boyunca" -> interval Closed (cycleNth TG.Week 0) end
                      "haftanın geri kalanı" -> interval Open today end
                      "bu hafta" -> interval Open today end
                      _ -> Nothing
       in case Text.toLower match of
         "bu hafta" -> Token Time . mkLatent <$> period
         _ -> Token Time <$> period
     _ -> Nothing
 }


ruleWeekend :: Rule
ruleWeekend = Rule
  { name = "week-end"
  , pattern =
    [ regex "hafta sonu"
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

ruleYYYYMMDD :: Rule
ruleYYYYMMDD = Rule
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

ruleNextNCycle :: Rule
ruleNextNCycle = Rule
  { name = "next n <cycle>"
  , pattern =
    [ regex "önümüzdeki|sonraki"
    , Predicate $ isIntegerBetween 1 9999
    , dimension TimeGrain
    ]
  , prod = \case
      (_:token:Token TimeGrain grain:_) -> do
        v <- getIntValue token
        tt $ cycleN True grain v
      _ -> Nothing
  }

ruleThisPartofday :: Rule
ruleThisPartofday = Rule
  { name = "this <part-of-day>"
  , pattern =
    [ regex "bu"
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
    [ regex "bu"
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
    [ regex "bu"
    , Predicate isOkWithThisNext
    ]
  , prod = \case
      (_:Token Time td:_) -> tt $ predNth 0 False td
      _ -> Nothing
  }

ruleNextTime :: Rule
ruleNextTime = Rule
  { name = "next <time>"
  , pattern =
    [ regex "önümüzdeki|gelecek|sonraki"
    , Predicate isOkWithThisNext
    ]
  , prod = \case
      (_:Token Time td:_) -> tt $ predNth 0 True td
      _ -> Nothing
  }

ruleLastTime :: Rule
ruleLastTime = Rule
  { name = "last <time>"
  , pattern =
    [ regex "geçen|önceki|geçtiğimiz|son"
    , Predicate isOkWithThisNext
    ]
  , prod = \case
      (_:Token Time td:_) -> tt $ predNth (- 1) False td
      _ -> Nothing
  }

ruleYearLatent :: Rule
ruleYearLatent = Rule
  { name = "year (latent)"
  , pattern =
    [ Predicate $
        or . sequence [isIntegerBetween (- 10000) 0, isIntegerBetween 25 999]
    ]
  , prod = \case
      (token:_) -> do
        y <- getIntValue token
        tt $ mkLatent $ year y
      _ -> Nothing
  }

ruleAfterTimeofday :: Rule
ruleAfterTimeofday = Rule
  { name = "after <time-of-day>"
  , pattern =
    [ regex "sonraki|önümüzdeki"
    , Predicate isATimeOfDay
    ]
  , prod = \case
      (_:Token Time td:_) -> tt $ withDirection TTime.After td
      _ -> Nothing
  }

ruleNight :: Rule
ruleNight = Rule
  { name = "night"
  , pattern =
    [ regex "gece(ye|den)?"
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
    [ dimension Time
    , dimension Ordinal
    , dimension TimeGrain
    ]
  , prod = \case
      (Token Time td:Token Ordinal od:Token TimeGrain grain:_) ->
        tt $ cycleNthAfter True grain (TOrdinal.value od - 1) td
      _ -> Nothing
  }

ruleHhmm :: Rule
ruleHhmm = Rule
  { name = "hh:mm"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3]))[:.ч]([0-5]\\d)(?:час(ов|а|у)?|ч)?"
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
        tt $ mkLatent $ hourMinute False hh mm
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
    [ dimension Time
    , regex "son"
    , Predicate isADayOfWeek
    ]
  , prod = \case
      (
        Token Time td1:
        _:
        Token Time td2:
        _) -> tt $ predLastOf td1 td2
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

ruleIntervalForDurationFrom :: Rule
ruleIntervalForDurationFrom = Rule
  { name = "for <duration> from <time>"
  , pattern =
    [ dimension Time
    , regex "itibaren"
    , dimension Duration
    , regex "boyunca|süresince"
    ]
  , prod = \case
      (Token Time td1:_:Token Duration dd:_) ->
        Token Time <$> interval TTime.Closed td1 (durationAfter dd td1)
      _ -> Nothing
}

ruleDurationAfterTime :: Rule
ruleDurationAfterTime = Rule
  { name = "<duration> after <time>"
  , pattern =
    [ dimension Time
    , regex "sonra(ki)?"
    , dimension Duration
    ]
  , prod = \case
      (Token Time td:_:Token Duration dd:_) ->
        tt $ durationAfter dd td
      _ -> Nothing
  }

ruleDurationAfterTime2 :: Rule
ruleDurationAfterTime2 = Rule
  { name = "<duration> after <time>"
  , pattern =
    [ dimension Time
    , dimension Duration
    , regex "sonra"
    ]
  , prod = \case
      (Token Time td:Token Duration dd:_:_) ->
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
      (Token Ordinal OrdinalData{TOrdinal.value = v}:_) ->
        tt $ cycleNthAfter False TG.Quarter (v - 1) $ cycleNth TG.Year 0
      _ -> Nothing
  }

ruleDurationBeforeTime :: Rule
ruleDurationBeforeTime = Rule
  { name = "<duration> before <time>"
  , pattern =
    [ dimension Time
    , regex "önceki"
    , dimension Duration
    ]
  , prod = \case
      (Token Time td:_:Token Duration dd:_) ->
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
  [ ruleAbsorptionOfAfterNamedDay
  , ruleAfterTimeofday
  , ruleAfternoon
  , ruleAtTimeofday
  , ruleAtTimeofday2
  , ruleBetweenTimeofdayAndTimeofdayInterval
  , ruleDatetimeDatetimeInterval
  , ruleDateDateInterval
  , ruleDayofmonthNonOrdinalNamedmonth
  , ruleDayofmonthOrdinal
  , ruleDayofmonthordinalNamedmonth
  , ruleDurationAfterTime
  , ruleDurationAfterTime2
  , ruleIntervalForDurationFrom
  , ruleDurationAgo
  , ruleDurationBeforeTime
  , ruleEvening
  , ruleHhmm
  , ruleHhmmMilitary
  , ruleHourofdayIntegerAsRelativeMinutes
  , ruleIdesOfMonth
  , ruleInDuration
  , ruleIntervalBy
  , ruleDurationHence
  , ruleDurationFromNow
  , ruleIntersect
  , ruleIntersectBy
  , ruleLastCycle
  , ruleLastCycleOfTime
  , ruleLastDayofweekOfTime
  , ruleLastNCycle
  , ruleLastTime
  , ruleThisTime
  , ruleNextTime
  , ruleLunch
  , ruleMidnighteodendOfDay
  , ruleMMDD
  , ruleMMDDYYYY
  , ruleYYYYMMDD
  , ruleNamedmonthDayofmonthNonOrdinal
  , ruleNextCycle
  , ruleNextNCycle
  , ruleNight
  , ruleNoon
  , ruleNthTimeAfterTime
  , ruleNthTimeOfTime
  , ruleOrdinalCycleOfTime
  , ruleOrdinalQuarter
  , ruleOrdinalQuarterYear
  , rulePartOfDays
  , rulePartofdayOfTime
  , rulePrecisionTOD
  , rulePrecisionTOD2
  , ruleThisCycle
  , ruleThisPartofday
  , ruleThisTime
  , ruleThisnextDayofweek
  , ruleTimePartofday
  , ruleTimeofdayLatent
  , ruleTimeofdayTimeofdayInterval
  , ruleTimeofdayTimeofdayInterval2
  , ruleUntilTimeofday
  , ruleWeek
  , ruleWeekend
  , ruleYear
  , ruleYearLatent
  , ruleYearLatent2
  , ruleTimezone
  ]
  ++ ruleInstants
  ++ ruleDaysOfWeek
  ++ ruleMonths
  ++ ruleSeasons
  ++ ruleHolidays
  ++ ruleComputedHolidays
