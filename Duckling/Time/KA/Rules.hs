-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.KA.Rules
  ( rules
  ) where

import Data.Maybe
import Data.Text (Text)
import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers (duration)
import Duckling.Duration.Types (DurationData (..))
import Duckling.Numeral.Helpers (isNatural, parseInt)
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Ordinal.Types (OrdinalData (..))
import Duckling.Regex.Types
import Duckling.Time.Computed
import Duckling.Time.Helpers
import Duckling.Time.Types (TimeData (..))
import Duckling.Types
import qualified Duckling.Duration.Types as TDuration
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.Ordinal.Types as TOrdinal
import qualified Duckling.Time.Types as TTime
import qualified Duckling.TimeGrain.Types as TG

ruleIntersect :: Rule
ruleIntersect = Rule
  { name = "intersect"
  , pattern =
    [ Predicate isNotLatent
    , Predicate $ or . sequence [isNotLatent, isGrainOfTime TG.Year]
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:Token Time td2:_) ->
        Token Time . notLatent <$> intersect td1 td2
      _ -> Nothing
  }

ruleAbsorbInMonthYear :: Rule
ruleAbsorbInMonthYear = Rule
  { name = "in <named-month>|year"
  , pattern =
    [ Predicate $ or . sequence [isAMonth, isGrainOfTime TG.Year]
    , regex " ?წელს| ?წელი| ?წლის| ?თვეს| ?თვის| ?თვის|-ში|ში"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleInstants :: [Rule]
ruleInstants = mkRuleInstants
  [ ("ახლა"    , TG.Second, 0  , "ახლავე|ეხლავე|ეხლა|ახლა|ამ წამს|ამ წუთას|ამ მომენტისთვი")
  , ("დღეს"        , TG.Day   , 0  , "დღეს"           )
  , ("ხვალ"     , TG.Day   , 1  , "ხვალე?"            )
  , ("ზეგ"     , TG.Day   , 2  , "ზეგე?"            )
  , ("გუშინ"    , TG.Day   , - 1, "გუშინ?"                      )
  , ("გუშინ წინ"    , TG.Day   , - 2, "გუშინ ?წინ"                      )
  ]

ruleNow :: Rule
ruleNow = Rule
  { name = "now"
  , pattern =
    [ regex "ახლავე|ეხლავე|ახლა|ეხლა|ამ წუთას|ამ მომენტისთვის"
    ]
  , prod = \_ -> tt now
  }
  
ruleNextDOW :: Rule
ruleNextDOW = Rule
  { name = "this|next <day-of-week>"
  , pattern =
    [ regex "შემდეგი|მომავალი"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth 1 True td
      _ -> Nothing
  }

ruleThisTime :: Rule
ruleThisTime = Rule
  { name = "this <time>"
  , pattern =
    [ regex "ამ|ეს|ახლანდელი|მიმდინარე"
    , Predicate isOkWithThisNext
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth 0 False td
      _ -> Nothing
  }

ruleNextTime :: Rule
ruleNextTime = Rule
  { name = "next <time>"
  , pattern =
    [ regex "შემდეგი?|მომავალი?"
    , Predicate isOkWithThisNext
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth 1 True td
      _ -> Nothing
  }

ruleLastTime :: Rule
ruleLastTime = Rule
  { name = "last <time>"
  , pattern =
    [ regex "(წინა|ბოლო|გასული?)"
    , Predicate isOkWithThisNext
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth (- 1) False td
      _ -> Nothing
  }

ruleLastWeekendOfMonth :: Rule
ruleLastWeekendOfMonth = Rule
  { name = "last weekend of <named-month>"
  , pattern =
    [ regex "ბოლო უიქ?კ? ?ენდი|ვიქ?კ? ?ენდი|შაბ?ფ?ათკვირა|შაბ?ფ?ათ-კვირა|უქმეები"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td2:_) -> tt $ predLastOf weekend td2
      _ -> Nothing
  }

ruleLastWorkweekOfMonth :: Rule
ruleLastWorkweekOfMonth = Rule
  { name = "last workweek of <named-month>"
  , pattern =
    [ regex "ბოლო სამუშაო ?კვირას?(ში)?"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td2:_) -> tt $ predLastOf workweek td2
      _ -> Nothing
  }

ruleLastWeekendOfMonth1 :: Rule
ruleLastWeekendOfMonth1 = Rule
  { name = "last weekend of <named-month>"
  , pattern =
    [ Predicate isAMonth
    , regex "(ს? ?ბოლო უიქ?კ? ?ენდი ?|ს? ბოლო ?ვიქ?კ? ?ენდი ?|ს? ბოლო ?შაბ?ფ?ათკვირა ?|ს? ბოლო ?შაბ?ფ?ათ-კვირა ?)| ს?უქმეები ?" 
    ]
  , prod = \tokens -> case tokens of
      (Token Time td2:Token RegexMatch (GroupMatch (match:_)):_) -> 
        tt $ predLastOf weekend td2
      _ -> Nothing
  }

ruleLastWorkweekOfMonth1 :: Rule
ruleLastWorkweekOfMonth1 = Rule
  { name = "last workweek of <named-month>"
  , pattern =
    [ Predicate isAMonth
    , regex "(ს? ბოლო სამუშაო ?კვირას?(ში)?)" 
    ]
  , prod = \tokens -> case tokens of
      (Token Time td2:Token RegexMatch (GroupMatch (match:_)):_) -> 
        tt $ predLastOf workweek td2
      _ -> Nothing
  }

ruleTimeAfterNext :: Rule
ruleTimeAfterNext = Rule
  { name = "<time> after next"
  , pattern =
    [ regex "(შემდეგის შემდეგი? ?)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):Token Time td:_) ->
        tt $ predNth 2 (Text.toLower match == "შემდეგის შემდეგ") td
      _ -> Nothing
  }

ruleTimeBeforeLast :: Rule
ruleTimeBeforeLast = Rule
  { name = "<time> before last"
  , pattern =
    [ regex "(ბოლოს წინა ?|წინის წინა ?)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):Token Time td:_) ->
        tt $ predNth (-2) (Text.toLower match == "ბოლოს წინა") td
      _ -> Nothing
  }

ruleLastDOWOfTime :: Rule
ruleLastDOWOfTime = Rule
  { name = "last <day-of-week> of <time>"
  , pattern =
    [ dimension Time
    , regex "ბოლო ?"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (Token Time td2:_:Token Time td1:_) ->
        tt $ predLastOf td1 td2
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
  , prod = \tokens -> case tokens of
      (Token Time td1:Token Ordinal od:Token Time td2:_) -> Token Time .
        predNth (TOrdinal.value od - 1) False <$> intersect td2 td1
      _ -> Nothing
  }

ruleNthTimeAfterTime :: Rule
ruleNthTimeAfterTime = Rule
  { name = "nth <time> after <time>"
  , pattern =
    [ dimension Time
    , regex "დან"
    , dimension Ordinal
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal od:Token Time td1:_:Token Time td2:_) ->
        tt $ predNthAfter (TOrdinal.value od - 1) td1 td2
      _ -> Nothing
  }

ruleYearLatent :: Rule
ruleYearLatent = Rule
  { name = "year (latent)"
  , pattern =
      [ Predicate $
        or . sequence [isIntegerBetween (- 10000) 0, isIntegerBetween 25 10000]
      ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ year n
      _ -> Nothing
  }

ruleTheDOMNumeral :: Rule
ruleTheDOMNumeral = Rule
  { name = "the <day-of-month> (number)"
  , pattern =
    [ Predicate isDOMInteger
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ dayOfMonth n
      _ -> Nothing
  }

ruleMonthDOMNumeral :: Rule
ruleMonthDOMNumeral = Rule
  { name = "<named-month> <day-of-month> (non ordinal)"
  , pattern =
    [ Predicate isAMonth
    , Predicate isDOMOrdinal
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:token:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleMonthDOMNumeral1 :: Rule
ruleMonthDOMNumeral1 = Rule
  { name = "<named-month> <day-of-month> (non ordinal)"
  , pattern =
    [ Predicate isAMonth
    , Predicate isDOMOrdinal
    , regex "( დღეს?)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:token:_) -> Token Time <$> intersectDOM td token
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
        tt . mkLatent $ hour True n
      _ -> Nothing
  }

ruleAtTOD :: Rule
ruleAtTOD = Rule
  { name = "at <time-of-day>"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "ს|-ს|-ზე|ზე| ზე" 
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleTODOClock :: Rule
ruleTODOClock = Rule
  { name = "<time-of-day> o'clock"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "სა?ათი?ს?(ზე)?(ისთვის)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
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

ruleHHMMLatent :: Rule
ruleHHMMLatent = Rule
  { name = "hhmm (latent)"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3]))([0-5]\\d)(?!.\\d)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (hh:mm:_)):_) -> do
        h <- parseInt hh
        m <- parseInt mm
        tt . mkLatent $ hourMinute True h m
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

ruleMilitaryAMPM :: Rule
ruleMilitaryAMPM = Rule
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

ruleMilitarySpelledOutAMPM :: Rule
ruleMilitarySpelledOutAMPM = Rule
  { name = "military spelled out numbers am|pm"
  , pattern =
    [ Predicate $ isIntegerBetween 10 12
    , Predicate $ isIntegerBetween 1 59
    , regex "([ap])(\\s|\\.)?m?\\.?"
    ]
    , prod = \tokens -> case tokens of
        (h:m:Token RegexMatch (GroupMatch (_:ap:_)):_) -> do
          hh <- getIntValue h
          mm <- getIntValue m
          tt . timeOfDayAMPM (Text.toLower ap == "a") $ hourMinute True hh mm
        _ -> Nothing
  }

ruleMilitarySpelledOutAMPM2 :: Rule
ruleMilitarySpelledOutAMPM2 = Rule
  { name = "six thirty six a.m."
  , pattern =
    [ Predicate $ isIntegerBetween 110 999
    , regex "([ap])(\\s|\\.)?m?\\.?"
    ]
  , prod = \tokens -> case tokens of
      (token:Token RegexMatch (GroupMatch (_:ap:_)):_) -> do
        n <- getIntValue token
        m <- case mod n 100 of
          v | v < 60 -> Just v
          _          -> Nothing
        let h = quot n 100
        tt . timeOfDayAMPM (Text.toLower ap == "a") $ hourMinute True h m
      _ -> Nothing
  }

ruleTODAMPM :: Rule
ruleTODAMPM = Rule
  { name = "<time-of-day> am|pm"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "(in the )?([ap])(\\s|\\.)?(m?)\\.?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td@TimeData {TTime.latent = True}:
       Token RegexMatch (GroupMatch (_:ap:_:"":_)):_) ->
        tt . mkLatent $ timeOfDayAMPM (Text.toLower ap == "a") td
      (Token Time td:Token RegexMatch (GroupMatch (_:ap:_)):_) ->
        tt $ timeOfDayAMPM (Text.toLower ap == "a") td
      _ -> Nothing
  }

ruleHONumeral :: Rule
ruleHONumeral = Rule
  { name = "<hour-of-day> <integer>"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isAnHourOfDay]
    , Predicate $ isIntegerBetween 1 59
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       token:
       _) -> do
        n <- getIntValue token
        tt $ hourMinute is12H hours n
      _ -> Nothing
  }

ruleNumeralToHOD :: Rule
ruleNumeralToHOD = Rule
  { name = "<integer> to|till|before <hour-of-day>"
  , pattern =
    [ Predicate isAnHourOfDay
    , Predicate $ isIntegerBetween 1 59
    , regex "(წუთი ?)აკლ(ია)?(და)?|(წუთი ?)უკლია"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:token:_) -> do
        n <- getIntValue token
        t <- minutesBefore n td
        Just $ Token Time t
      _ -> Nothing
  }

ruleNumeralToHOD1 :: Rule
ruleNumeralToHOD1 = Rule
  { name = "<integer> to|till|before <hour-of-day>"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "( ?რო ?)?"
    , Predicate $ isIntegerBetween 1 59
    , regex "(წუთი ?)აკლ(ია)?(და)?|(წუთი ?)უკლია"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_:token:_) -> do
        n <- getIntValue token
        t <- minutesBefore n td
        Just $ Token Time t
      _ -> Nothing
  }

ruleHalfToHOD :: Rule
ruleHalfToHOD = Rule
  { name = "half to|till|before <hour-of-day>"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "ის ნახევა?რი?(ზე)?|-ის ნახევა?რი?(ზე)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> Token Time <$> minutesBefore 30 td
      _ -> Nothing
  }

ruleNumeralAfterHOD :: Rule
ruleNumeralAfterHOD = Rule
  { name = "integer after|past <hour-of-day>"
  , pattern =
    [ Predicate isAnHourOfDay
    , Predicate $ isIntegerBetween 1 59
    , regex "წუთი?(ზე)?(სთვის)?" 
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:token:_) -> do
        n <- getIntValue token
        t <- minutesBefore (60-n) td
        Just $ Token Time t
      _ -> Nothing
  }

ruleNumeralAfterHOD1 :: Rule
ruleNumeralAfterHOD1 = Rule
  { name = "integer after|past <hour-of-day>"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "-ის|ის|საათ(სა)?(ზე)?(და )?"
    , Predicate $ isIntegerBetween 1 59
    , regex "წუთი?(ზე)?(სთვის)?" 
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_:token:_) -> do
        n <- getIntValue token
        t <- minutesBefore (60-n) td
        Just $ Token Time t
      _ -> Nothing
  }

ruleNumeralAfterHOD2 :: Rule
ruleNumeralAfterHOD2 = Rule
  { name = "integer after|past <hour-of-day>"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "საათი?(სა)?(ზე)?( ?და ?)?"
    , Predicate $ isIntegerBetween 1 59
    , regex "წუთი?(ზე)?(სთვის)?" 
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_:token:_) -> do
        n <- getIntValue token
        t <- minutesAfter n td
        Just $ Token Time t
      _ -> Nothing
  }

ruleMMYYYY :: Rule
ruleMMYYYY = Rule
  { name = "mm/yyyy"
  , pattern =
    [ regex "(0?[1-9]|1[0-2])[/-](\\d{4})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (mm:yy:_)):_) -> do
        y <- parseInt yy
        m <- parseInt mm
        tt $ yearMonthDay y m 1
      _ -> Nothing
  }

ruleYYYYMMDD :: Rule
ruleYYYYMMDD = Rule
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

ruleNoonMidnightEOD :: Rule
ruleNoonMidnightEOD = Rule
  { name = "noon|midnight|EOD|end of day"
  , pattern =
    [ regex "((შუა)?ღამე?(ით)?ი?ს?)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> tt . hour False $
        if Text.toLower match == "noon" then 12 else 0
      _ -> Nothing
  }

rulePartOfDays :: Rule
rulePartOfDays = Rule
  { name = "part of days"
  , pattern =
    [ regex "(გვიან ღამე?(ით)?|დილა?ს?(ის)?(ით)?|საღამოს?(თი)?|(შუა)?ღამე?(ით)?ი?ს?(ისას)?|შუადღის|შუადღით|დღის|დღით)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        let (start, end) = case Text.toLower match of
              "დილას"  -> (hour False 4, hour False 12)
              "დილის"  -> (hour False 4, hour False 12)
              "დილით"  -> (hour False 4, hour False 12)
              "დილა"  -> (hour False 4, hour False 12)
              "დილ"  -> (hour False 4, hour False 12)
              "საღამოთი"  -> (hour False 18, hour False 0)
              "საღამოს"  -> (hour False 18, hour False 0)
              "საღამო"  -> (hour False 18, hour False 0)
              "შუაღამით"    -> (hour False 18, hour False 0)
              "შუაღამისას"    -> (hour False 18, hour False 0)
              "შუაღამის"    -> (hour False 18, hour False 0)
              "შუაღამე"    -> (hour False 18, hour False 0)
              "ღამით"    -> (hour False 18, hour False 0)
              "ღამის"    -> (hour False 18, hour False 0)
              "ღამე"    -> (hour False 18, hour False 0)
              "გვიან ღამე"    -> (hour False 21, hour False 0)
              "გვიან ღამით"    -> (hour False 21, hour False 0)
              "შუადღე"    -> (hour False 12, hour False 18)
              "შუადღის"    -> (hour False 12, hour False 18)
              "შუადღით"    -> (hour False 12, hour False 18)
              "დღისით"    -> (hour False 12, hour False 18)
              "დღით"    -> (hour False 12, hour False 18)
              "დღის"    -> (hour False 12, hour False 18)
              _          -> (hour False 12, hour False 19)
        td <- interval TTime.Open start end
        tt . partOfDay $ mkLatent td
      _ -> Nothing
  }

ruleEarlyMorning :: Rule
ruleEarlyMorning = Rule
  { name = "early morning"
  , pattern =
    [ regex "დილაუთენია|დილა უთენია"
    ]
  , prod = \_ -> Token Time . partOfDay . mkLatent <$>
      interval TTime.Open (hour False 4) (hour False 9)
  }

rulePODIn :: Rule
rulePODIn = Rule
  { name = "in|during the <part-of-day>"
  , pattern =
    [ Predicate isAPartOfDay
    , regex "(-ის )?(ის )?განმავლობაში"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

rulePODThis :: Rule
rulePODThis = Rule
  { name = "this <part-of-day>"
  , pattern =
    [ regex "დღეს ?|ამ ?"
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time . partOfDay . notLatent <$>
        intersect (cycleNth TG.Day 0) td
      _ -> Nothing
  }

ruleTonight :: Rule
ruleTonight = Rule
  { name = "tonight"
  , pattern = [regex "(დღეს )?(გვიან )?ღამე?(ით)?(ისას)?|(დღეს )?(შუა )?ღამე?(ით)?ი?ს?(ისას)?|საღამოს"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        let today = cycleNth TG.Day 0
            h = if Text.toLower match == "გვიან " then 21 else 18
        evening <- interval TTime.Open (hour False h) (hour False 0)
        Token Time . partOfDay . notLatent <$> intersect today evening
      _ -> Nothing
  }

ruleAfterPartofday :: Rule
ruleAfterPartofday = Rule
  { name = "after lunch/work/school"
  , pattern =
    [ regex "სკოლის მერე"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        (start, end) <- case Text.toLower match of
          "ლანჩის მერე"  -> Just (hour False 13, hour False 17)
          "ლანჩის შემდეგ"  -> Just (hour False 13, hour False 17)
          "სამსახურის მერე"  -> Just (hour False 17, hour False 21)
          "სამსახურის შემდეგ"  -> Just (hour False 17, hour False 21)
          "სკოლის მერე"  -> Just (hour False 15, hour False 21)
          "სკოლის შემდეგ"  -> Just (hour False 15, hour False 21)
          _        -> Nothing
        td <- interval TTime.Open start end
        Token Time . partOfDay . notLatent <$>
          intersect (cycleNth TG.Day 0) td
      _ -> Nothing
  }

-- Since part of days are latent, general time intersection is blocked
ruleTimePOD :: Rule
ruleTimePOD = Rule
  { name = "<time> <part-of-day>"
  , pattern =
    [ dimension Time
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:Token Time pod:_) -> Token Time <$> intersect pod td
      _ -> Nothing
  }

rulePODofTime :: Rule
rulePODofTime = Rule
  { name = "<part-of-day> <time>"
  , pattern =
    [ Predicate isAPartOfDay
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Time pod:Token Time td:_) -> Token Time <$> intersect pod td
      _ -> Nothing
  }

ruleWeekend :: Rule
ruleWeekend = Rule
  { name = "week-end"
  , pattern =
    [ regex "(მიმდინარე )?ვიქ?კ? ?ენდი?(ზე)?(სას)?|(მიმდინარე )?უიქ?კ? ?ენდი?(ზე)?(სას)?|(მიმდინარე )?შაბ?ფ?ათ ?-?კვირას?|(მიმდინარე )?უქმეები?(ზე)?"
    ]
  , prod = \_ -> tt $ mkOkForThisNext weekend
  }

ruleworkweek :: Rule
ruleworkweek = Rule
  { name = "work-week"
  , pattern =
    [ regex "(მიმდინარე )?სამუშაო ?კვირას?(ში)?"
    ]
  , prod = \_ -> tt $ mkOkForThisNext workweek
  }

ruleSeason :: Rule
ruleSeason = Rule
  { name = "last|this|next <season>"
  , pattern =
    [ regex "(ამ|ეს|ახლანდელი|მიმდინარე|შემდეგი|მომავალი|წინა|ბოლო|გასული?) სეზონი?(ზე)?"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        n <- case Text.toLower match of
               "ეს" -> Just 0
               "ამ" -> Just 0
               "ახლანდელი" -> Just 0
               "მიმდინარე" -> Just 0
               "წინა" -> Just (-1)
               "ბოლო" -> Just (-1)
               "გასული" -> Just (-1)
               "გასულ" -> Just (-1)
               "შემდეგი" -> Just 1
               "მომავალი" -> Just 1
               _ -> Nothing
        tt $ predNth n False season
      _ -> Nothing
  }

ruleSeasons :: [Rule]
ruleSeasons = mkRuleSeasons
  [ ( "ზაფხული", "ზაფხული?(ში)?ს?"     , monthDay  6 1, monthDay  8 31 )
  , ( "შემოდგომა"  , "შემოდგომა?(ში)?ს?", monthDay  9 1, monthDay 11 30 )
  , ( "ზამთარი", "ზამთარი?(ში)?ს?"     , monthDay 12 1, monthDay  2 28 )
  , ( "გაზაფხული", "გაზაფხული?(ში)?ს?"     , monthDay  3 1, monthDay  5 31 )
  ]

ruleTODPrecision :: Rule
ruleTODPrecision = Rule
  { name = "<time-of-day> sharp|exactly"
  , pattern =
    [ regex "(ზუსტად|იმენა)"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

rulePrecisionTOD :: Rule
rulePrecisionTOD = Rule
  { name = "about|exactly <time-of-day>"
  , pattern =
    [ regex "დაახლოებით|გძეტა"
    , Predicate $ isGrainFinerThan TG.Year
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleIntervalMonthDDDD :: Rule
ruleIntervalMonthDDDD = Rule
  { name = "<month> dd-dd (interval)"
  , pattern =
    [ Predicate isAMonth
    , Predicate isDOMValue
    , regex "-დან|დან| დან"
    , Predicate isDOMValue
    , regex "-მდე|მდე| მდე"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:
       token1:
       _:
       token2:
       _) -> do
        dom1 <- intersectDOM td token1
        dom2 <- intersectDOM td token2
        Token Time <$> interval TTime.Closed dom1 dom2
      _ -> Nothing
  }

ruleIntervalMonthDDDD1 :: Rule
ruleIntervalMonthDDDD1 = Rule
  { name = "<month> dd-dd (interval)"
  , pattern =
    [ Predicate isAMonth
    , Predicate isDOMValue
    , regex "-"
    , Predicate isDOMValue
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:
       token1:
       _:
       token2:
       _) -> do
        dom1 <- intersectDOM td token1
        dom2 <- intersectDOM td token2
        Token Time <$> interval TTime.Closed dom1 dom2
      _ -> Nothing
  }

ruleIntervalDDDDMonth :: Rule
ruleIntervalDDDDMonth = Rule
  { name = "dd-dd <month> (interval)"
  , pattern =
    [ Predicate isDOMValue
    , regex "-დან|დან| დან"
    , Predicate isDOMValue
    , regex "-მდე|მდე| მდე"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (token1:
       _:
       token2:
       _:
       Token Time td:
       _) -> do
        dom1 <- intersectDOM td token1
        dom2 <- intersectDOM td token2
        Token Time <$> interval TTime.Closed dom1 dom2
      _ -> Nothing
  }

ruleIntervalDDDDMonth1 :: Rule
ruleIntervalDDDDMonth1 = Rule
  { name = "dd-dd <month> (interval)"
  , pattern =
    [ Predicate isDOMValue
    , regex "-"
    , Predicate isDOMValue
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (token1:
       _:
       token2:
       Token Time td:
       _) -> do
        dom1 <- intersectDOM td token1
        dom2 <- intersectDOM td token2
        Token Time <$> interval TTime.Closed dom1 dom2
      _ -> Nothing
  }

ruleIntervalBetweenMM :: Rule
ruleIntervalBetweenMM = Rule
  { name = "between <time> and <time>"
  , pattern =
    [ Predicate isDOMValue
    , regex "-ი?დან ?|ი?დან ?| ი?დან ?"
    , Predicate isDOMValue
    , Predicate isAMonth
    , regex "-ა?მდე ?|ა?მდე ?| ა?მდე ?"
    ]
  , prod = \tokens -> case tokens of
      (token1:_:token2:Token Time td:_) -> do
        start <- intersectDOM td token1
        end <- intersectDOM td token2
        Token Time <$> interval TTime.Closed start end
      _ -> Nothing
  }

ruleIntervalBetween :: Rule
ruleIntervalBetween = Rule
  { name = "between <time> and <time>"
  , pattern =
    [ dimension Time
    , regex "-ი?დან ?|ი?დან ?| ი?დან ?"
    , dimension Time
    , regex "-ა?მდე ?|ა?მდე ?| ა?მდე ?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleIntervalBetween1 :: Rule
ruleIntervalBetween1 = Rule
  { name = "between <time> and <time>"
  , pattern =
    [ dimension Time
    , regex "-ი?დან ?|ი?დან ?| ი?დან ?"
    , regex "დღემდე|ამ წუთამდე|ახლამდე|ამ მომენტამდე"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:_:_) ->
        Token Time <$> interval TTime.Closed td1 now
      _ -> Nothing
  }

ruleIntervalBetween2 :: Rule
ruleIntervalBetween2 = Rule
  { name = "between <time-of-day> and <time-of-day>"
  , pattern =
    [ dimension Time
    , Predicate isAPartOfDay
    , regex "დან|-ი?დან ?|ი?დან ?| ი?დან ?"
    , dimension Time
    , Predicate isAPartOfDay
    , regex "მდე|-ა?მდე ?|ა?მდე ?| ა?მდე ?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:Token Time pod1:_:Token Time td2:Token Time pod2:_) -> do
        dom1 <- intersect pod1 td1
        dom2 <- intersect pod2 td2
        Token Time <$> interval TTime.Closed dom1 dom2
      _ -> Nothing
  }

-- Specific for time-of-day, to help resolve ambiguities
ruleIntervalTODDash :: Rule
ruleIntervalTODDash = Rule
  { name = "<time-of-day> - <time-of-day> (interval)"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isATimeOfDay]
    , regex "დან|-ი?დან ?|ი?დან ?| ი?დან ?"
    , Predicate isATimeOfDay
    , regex "მდე|-ა?მდე ?|ა?მდე ?| ა?მდე ?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleIntervalTODFrom :: Rule
ruleIntervalTODFrom = Rule
  { name = "from <time-of-day> - <time-of-day> (interval)"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "დან|-ი?დან ?|ი?დან ?| ი?დან ?"
    , Predicate isATimeOfDay
    , regex "მდე|-ა?მდე ?|ა?მდე ?| ა?მდე ?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleIntervalTODBetween :: Rule
ruleIntervalTODBetween = Rule
  { name = "between <time-of-day> and <time-of-day> (interval)"
  , pattern =
    [ Predicate isATimeOfDay
    , regex " ?და ?"
    , Predicate isATimeOfDay
    , regex "შორის"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleIntervalBy :: Rule
ruleIntervalBy = Rule
  { name = "by <time>"
  , pattern =
    [ dimension Time
    , regex "მდე|-ა?მდე ?|ა?მდე ?| ა?მდე ?"
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        Token Time <$> interval TTime.Open (cycleNth TG.Second 0) td
      _ -> Nothing
  }

ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = mkRuleDaysOfWeek
  [ ( "ორშაბათი"   , "ორშაბათი?ს?|ორშ?\\.?"         )
  , ( "სამშაბათი"  , "სამშაბათი?ს?|სამშ?\\.?"      )
  , ( "ოთხშაბათი", "ოთხშაბათი?ს?|ოთხშ?\\.?"     )
  , ( "ხუთშაბათი" , "ხუთშაბათი?ს?|ხუთშ?\\.?" )
  , ( "პარასკევი"   , "პარასკევი?ს?|პარ\\.?"         )
  , ( "შაბათი" , "შაბათი?ს?|შაბ\\.?"       )
  , ( "კვირა"   , "კვირას?|კვირის|კვრ?\\.?"         )
  ]

ruleMonths :: [Rule]
ruleMonths = mkRuleMonthsWithLatent
  [ ( "იანვარი"  , "იანვა?რი?ს?(ის)?|იან\\.?"    , False )
  , ( "თებერვალი" , "თებერვა?ლი?ს?(ის)?|თებ\\.?"   , False )
  , ( "მარტი"    , "მარტი?ს?(ის)?|მარ\\.?"      , False )
  , ( "აპრილი"    , "აპრილი?ს?(ის)?|აპრ\\.?"      , False )
  , ( "მაისი"      , "მაისი?ს?(ის)?"                , False  )
  , ( "ივნისი"     , "ივნისი?ს?(ის)? ?|ივნ\\.?"       , False )
  , ( "ივლისი"     , "ივლისი?ს?(ის)?|ივლ\\.?"       , False )
  , ( "აგვისტო"   , "აგვისტო?ს?|აგვ\\.?"     , False )
  , ( "სექტემბერი", "სექტემბე?რის|სექტემბე?რი?ს?|სექტ?\\.?", False )
  , ( "ოქტომბერი"  , "ოქტომბე?რის|ოქტომბე?რი?ს?|ოქტ\\.?"    , False )
  , ( "ნოემბერი" , "ნოემბე?რის|ნოემბე?რი?ს?|ნოე\\.?"   , False )
  , ( "დეკემბერი" , "დეკემბრის?|დეკემბე?რი?ს?|დეკ\\.?"   , False )
  ]

ruleNamedMonth :: Rule
ruleNamedMonth = Rule
  { name = "in <named-month>"
  , pattern =
    [ Predicate isAMonth
    , regex " ?ში"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td2:_) -> Token Time <$> interval TTime.Closed (cycleNthAfter False TG.Month (0) td2) (cycleNthAfter False TG.Month (0) td2)
      _ -> Nothing
  }

rulePartOfMonth :: Rule
rulePartOfMonth = Rule
  { name = "part of <named-month>"
  , pattern =
    [ Predicate isAMonth
    , regex "(დასაწყისი?(ში)?(სას)?(სკენ)?|შუა?(ში)?(ისკენ)?|ბოლოს?(მდე)?(ში)?(სკენ)?)-?( of)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:Token RegexMatch (GroupMatch (match:_)):_) -> do
        (sd, ed) <- case Text.toLower match of
          "დასაწყისისკენ" -> Just (1, 10)
          "დასაწყისიდან" -> Just (1, 10)
          "დასაწყისას" -> Just (1, 10)
          "დასაწყისში" -> Just (1, 10)
          "დასაწყისი" -> Just (1, 10)
          "შუისკენ"   -> Just (11, 20)
          "შუაში"   -> Just (11, 20)
          "შუა"   -> Just (11, 20)
          "ბოლომდე"  -> Just (21, -1)
          "ბოლოსკენ"  -> Just (21, -1)
          "ბოლოში"  -> Just (21, -1)
          "ბოლოს"  -> Just (21, -1)
          "ბოლო"  -> Just (21, -1)
          _       -> Nothing
        start <- intersect td $ dayOfMonth sd
        end <- if ed /= -1
          then intersect td $ dayOfMonth ed
          else Just $ cycleLastOf TG.Day td
        Token Time <$> interval TTime.Open start end
      _ -> Nothing
  }

rulePartOfMonth1 :: Rule
rulePartOfMonth1 = Rule
  { name = "part of <named-month>"
  , pattern =
    [ regex "(დასაწყისი?|შუა|ბოლოს?)-?( of)?"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):Token Time td:_) -> do
        (sd, ed) <- case Text.toLower match of
          "დასაწყისი" -> Just (1, 10)
          "დასაწყის" -> Just (1, 10)
          "შუა"   -> Just (11, 20)
          "ბოლო"  -> Just (21, -1)
          "ბოლოს"  -> Just (21, -1)
          _       -> Nothing
        start <- intersect td $ dayOfMonth sd
        end <- if ed /= -1
          then intersect td $ dayOfMonth ed
          else Just $ cycleLastOf TG.Day td
        Token Time <$> interval TTime.Open start end
      _ -> Nothing
  }

ruleEndOrBeginningOfMonth :: Rule
ruleEndOrBeginningOfMonth = Rule
  { name = "at the beginning|end of <named-month>"
  , pattern =
    [ regex "(ის )?(დასაწყისშ?ი ?|ბოლოს? ?|ბოლოში ?)"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (_:match:_)):Token Time td:_) -> do
        (sd, ed) <- case Text.toLower match of
          "დასაწყისი" -> Just (1, 10)
          "დასაწყისში" -> Just (1, 10)
          "ბოლოს"       -> Just (21, -1)
          "ბოლოში"       -> Just (21, -1)
          "ბოლო"       -> Just (21, -1)
          _           -> Nothing
        start <- intersect td $ dayOfMonth sd
        end <- if ed /= -1
          then intersect td $ dayOfMonth ed
          else Just $ cycleLastOf TG.Day td
        Token Time <$> interval TTime.Open start end
      _ -> Nothing
  }

ruleEndOfMonth :: Rule
ruleEndOfMonth = Rule
  { name = "end of month"
  , pattern = [ regex "(ამ )?(მიმდინარე )?თვის ბოლოს?(კენ)?(თვის)?" ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_)
        | (Just start, Just end) <- parsed ->
          Token Time <$> interval TTime.Open start end
        where
          cycleMonth = cycleNth TG.Month
          parsed = if "by" `Text.isPrefixOf` Text.toLower match
            then
              ( Just $ cycleNth TG.Second 0
              , intersect (dayOfMonth 1) $ cycleMonth 1)
            else
              ( intersect (dayOfMonth 21) $ cycleMonth 0
              , Just $ cycleLastOf TG.Day $ cycleMonth 0)
      _ -> Nothing
  }

ruleBeginningOfMonth :: Rule
ruleBeginningOfMonth = Rule
  { name = "beginning of month"
  , pattern = [ regex "(ამ )?(მიმდინარე )?თვის დასაწყისი?(ში)?(სკენ)?(სთვის)?" ]
  , prod = \_ -> do
      start <- intersect (dayOfMonth 1) $ cycleNth TG.Month 0
      end <- intersect (dayOfMonth 10) $ cycleNth TG.Month 0
      Token Time <$> interval TTime.Open start end
  }

ruleEndOrBeginningOfYear :: Rule
ruleEndOrBeginningOfYear = Rule
  { name = "at the beginning|end of <year>"
  , pattern =
    [ Predicate $ isGrainOfTime TG.Year
    , regex "(წლის )?დასაწყისი?(ისკენ)?(ში)? |ბოლოს?(კენ)?(ში)? "
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:Token RegexMatch (GroupMatch (_:match:_)):_) -> do
        (sd, ed) <- case Text.toLower match of
          "დასაწყისისკენ" -> Just (1, 4)
          "დასაწყისში" -> Just (1, 4)
          "ბოლოსკენ"       -> Just (9, -1)
          "ბოლოს"       -> Just (9, -1)
          "ბოლოში"       -> Just (9, -1)
          _           -> Nothing
        start <- intersect td $ month sd
        end <- if ed /= -1
          then intersect td $ cycleLastOf TG.Month $ month ed
          else cycleNthAfter False TG.Year 1 <$> intersect td (month 1)
        Token Time <$> interval TTime.Open start end
      _ -> Nothing
  }
  
ruleEndOfYear :: Rule
ruleEndOfYear = Rule
  { name = "end of year"
  , pattern = [ regex "((მიმდინარე )?(ამ )?წლის ბოლოს?(კენ)?(ში)?(თვის)?)" ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        start <- std
        end <- intersect (month 1) $ cycleYear 1
        Token Time <$> interval TTime.Open start end
          where
            std = if "by" `Text.isPrefixOf` Text.toLower match
              then Just $ cycleNth TG.Second 0
              else intersect (month 9) $ cycleYear 0
            cycleYear = cycleNth TG.Year
      _ -> Nothing
  }

ruleBeginningOfYear :: Rule
ruleBeginningOfYear = Rule
  { name = "beginning of year"
  , pattern = [ regex "((ამ )?(მიმდინარე )?წლის დასაწყისი?(ში)?(სკენ)?(სთვის)?)" ]
  , prod = \_ -> do
      start <- intersect (month 1) $ cycleNth TG.Year 0
      end <- intersect (month 4) $ cycleNth TG.Year 0
      Token Time <$> interval TTime.Open start end
  }

ruleEndOrBeginningOfWeek :: Rule
ruleEndOrBeginningOfWeek = Rule
  { name = "at the beginning|end of <week>"
  , pattern =
    [ Predicate $ isGrainOfTime TG.Week
    , regex "(დასაწყისი|დასაწყისში|ბოლოში|ბოლოს)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:Token RegexMatch (GroupMatch (_:match1:_)):_) -> do
        (sd, ed) <- case Text.toLower match1 of
          "დასაწყისი" -> Just (1, 3)
          "დასაწყისში" -> Just (1, 3)
          "ბოლოში"   -> Just (5, 7)
          "ბოლოს"    -> Just (5, 7)
          _           -> Nothing
        start <- intersect td $ dayOfWeek sd
        end <- intersect td $ dayOfWeek ed
        Token Time <$> interval TTime.Open start end
      _ -> Nothing
  }

rulePeriodicHolidays :: [Rule]
rulePeriodicHolidays = mkRuleHolidays
  -- Fixed dates, year over year
  [ ( "ახალი წელი", "ახალი წე?ლი?ს?", monthDay 1 1 )
  , ( "შობა", "შობა?(ის)?ს?", monthDay 1 7 )
  , ( "ნათლისღება", "ნათლისღება?(ის)?ს?", monthDay 1 19 )
  , ( "დედის დღე", "დედის დღე?ს?", monthDay 3 3 )
  , ( "ქალთა საერთაშორისო დღე", "ქალთა (საერთაშორისო ?)?დღე?ს?", monthDay 3 8 )
  , ( "საქართველოს დამოუკიდებლობის აღდგენის დღე", "საქართველოს (თავისუფლებისა და ერთიანობისთვის დაღუპულთა მოხსენიების )?(დამოუკიდებლობის აღდგენის )?დღეს?", monthDay 4 9 )
  , ( "გამარჯვების დღე", "(ფაშიზმზე )?გამარჯვების დღეს?", monthDay 5 9 )
  , ( "წმინდა ანდრია პირველწოდებულის ხსენების დღე", "(წმინდა )?ანდრია პირველწოდებულის ხსენების დღე?ს?", monthDay 5 12 )
  , ( "დამოუკიდებლობის დღე", "(საქართველოს )?დამოუკიდებლობის დღე?ს?", monthDay 5 26 )
  , ( "მცხეთობა", "მცხეთობა?ს?", monthDay 10 14 )
  , ( "გიორგობა", "გიორგობა?ს?", monthDay 11 26 )
  ]

ruleComputedHolidays :: [Rule]
ruleComputedHolidays = mkRuleHolidays
  [ ( "Orthodox Easter Monday", "orthodox\\s+easter\\s+mon(day)?"
    , cycleNthAfter False TG.Day 1 orthodoxEaster )
  , ( "აღდგომა", "აღდგომი?ა?ს?(ისას)?"
    , orthodoxEaster )
  , ( "დიდი შაბათი", "დიდი? შაბათი?ს?(ისას)?"
    , cycleNthAfter False TG.Day (-1) orthodoxEaster )
  , ( "წითელი პარასკევი", "წითელი? პარასკევი?ს?(სას)?"
    , cycleNthAfter False TG.Day (-2) orthodoxEaster )
  , ( "დიდი ხუთშაბათი", "დიდი? ხუთშაბათი?ს?(ისას)?"
    , cycleNthAfter False TG.Day (-7) orthodoxEaster )
  ]

ruleCycleThisLastNext :: Rule
ruleCycleThisLastNext = Rule
  { name = "this|last|next <cycle>"
  , pattern =
    [ regex "(ეს|ამ|მიმდინარე|შემდეგი?|მომავალი?|წინა|წინის წინა?|გასული?)"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):Token TimeGrain grain:_) ->
        case Text.toLower match of
          "ეს"          -> Token Time <$> interval TTime.Closed (cycleNth grain 0) now
          "ამ"        -> Token Time <$> interval TTime.Closed (cycleNth grain 0) now
          "მიმდინარე"       -> Token Time <$> interval TTime.Closed (cycleNth grain 0) now
          "წინა"          -> Token Time <$> interval TTime.Open (cycleNth grain (-1)) (cycleNth grain 0)
          "გასული" -> Token Time <$> interval TTime.Open (cycleNth grain (-1)) (cycleNth grain 0)
          "გასულ" -> Token Time <$> interval TTime.Open (cycleNth grain (-1)) (cycleNth grain 0)
          "წინის წინ"          -> Token Time <$> interval TTime.Open (cycleNth grain (-2)) (cycleNth grain (-1))
          "წინის წინა"          -> Token Time <$> interval TTime.Open (cycleNth grain (-2)) (cycleNth grain (-1))
          "შემდეგ"          -> Token Time <$> interval TTime.Open (cycleNth grain 1) (cycleNth grain 2)
          "შემდეგი"          -> Token Time <$> interval TTime.Open (cycleNth grain 1) (cycleNth grain 2)
          "მომავალ"          -> Token Time <$> interval TTime.Open (cycleNth grain 1) (cycleNth grain 2)
          "მომავალი"          -> Token Time <$> interval TTime.Open (cycleNth grain 1) (cycleNth grain 2)
          _ -> Nothing
      _ -> Nothing
  }

ruleDOMOfTimeMonth :: Rule
ruleDOMOfTimeMonth = Rule
  { name = "<day-of-month> (ordinal or number) of <month>"
  , pattern =
    [ Predicate isDOMValue
    , Predicate $ isGrainOfTime TG.Month
    ]
  , prod = \tokens -> case tokens of
      (token:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleCycleTheAfterBeforeTime :: Rule
ruleCycleTheAfterBeforeTime = Rule
  { name = "the <cycle> after|before <time>"
  , pattern =
    [ dimension Time
    , regex "(დან| ?ა?მდე ?)"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (  Token Time td
       : Token RegexMatch (GroupMatch (match:_))
       : Token TimeGrain grain
       : _) ->
        let n = if Text.toLower match == "დან" then 1 else - 1 in
          tt $ cycleNthAfter False grain n td
      _ -> Nothing
  }

ruleCycleTheAfterBeforeTime1 :: Rule
ruleCycleTheAfterBeforeTime1 = Rule
  { name = "the <cycle> after <time>"
  , pattern =
    [ dimension Time
    , regex "დან"
    , Predicate $ isIntegerBetween 0 99999
    , dimension TimeGrain
    , regex "ში|ის შემდეგ"
    ]
  , prod = \tokens -> case tokens of
      (  Token Time td
       : _
       : token 
       : Token TimeGrain grain 
       : _) -> do
          n <- getIntValue token
          tt $ cycleNthAfter False grain n td
      _ -> Nothing
  }

ruleCycleOrdinalOfTime :: Rule
ruleCycleOrdinalOfTime = Rule
  { name = "<ordinal> <cycle> of <time>"
  , pattern =
    [ dimension Time
    , regex "დან"
    , dimension Ordinal
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_:token:Token TimeGrain grain:_) -> do
        n <- getIntValue token
        tt $ cycleNthAfter True grain (n - 1) td
      _ -> Nothing
  }

ruleCycleLastOrdinalOfTime :: Rule
ruleCycleLastOrdinalOfTime = Rule
  { name = "<ordinal> last <cycle> of <time>"
  , pattern =
    [ dimension Time
    , regex "ბოლოდან"
    , dimension Ordinal
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_:token:Token TimeGrain grain:_) -> do
        n <- getIntValue token
        tt . cycleNthAfter True grain (-n) . cycleNthAfter True (timeGrain td) 1 $ td
      _ -> Nothing
  }

ruleDurationInWithinAfter :: Rule
ruleDurationInWithinAfter = Rule
  { name = "in|within|after <duration>"
  , pattern =
    [ dimension Duration
    , regex "(განმავლობაში|შემდეგ)"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
         "განმავლობაში" -> Token Time <$>
           interval TTime.Open (cycleNth TG.Second 0) (inDuration dd)
         "შემდეგ"  -> tt . withDirection TTime.After $ inDuration dd
         _        -> Nothing
      _ -> Nothing
  }

ruleDurationLastNext :: Rule
ruleDurationLastNext = Rule
  { name = "last|past|next <duration>"
  , pattern =
    [ regex "(წინა|მომდევნო|გასული?)"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       Token Duration DurationData{TDuration.grain, TDuration.value}:
       _) -> case Text.toLower match of
         "მომდევნო" -> tt $ cycleN True grain value
         "წინა" -> tt $ cycleN True grain (- value)
         "გასული" -> tt $ cycleN True grain (- value)
         "გასულ" -> tt $ cycleN True grain (- value)
         _      -> Nothing
      _ -> Nothing
  } 

ruleNDOWago :: Rule
ruleNDOWago = Rule
  { name = "<integer> <named-day> ago|back"
  , pattern =
    [ Predicate isNatural
    , Predicate isADayOfWeek
    , regex " ?დღის წინ| ?დღის უკან"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:Token Time td:_) ->
        tt $ predNth (- (floor v)) False td
      _ -> Nothing
  }

ruleDurationHenceAgo :: Rule
ruleDurationHenceAgo = Rule
  { name = "<duration> hence|ago"
  , pattern =
    [ dimension Duration
    , regex "წინ|უკან"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
        "წინ" -> tt $ durationAgo dd
        "უკან" -> tt $ durationAgo dd
        _     -> tt $ inDuration dd
      _ -> Nothing
  }

ruleDayDurationHenceAgo :: Rule
ruleDayDurationHenceAgo = Rule
  { name = "<day> <duration> hence|ago"
  , pattern =
    [ Predicate $ or . sequence [isGrainOfTime TG.Day, isGrainOfTime TG.Month]
    , dimension Duration
    , regex "(წინ|უკან)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:
       Token Duration dd:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
         "უკან" -> Token Time <$> intersect td (durationIntervalAgo dd)
         _     -> Token Time <$> intersect td (inDurationInterval dd)
      _ -> Nothing
  }

ruleInNumeral :: Rule
ruleInNumeral = Rule
  { name = "in <number> (implicit minutes)"
  , pattern =
    [ Predicate $ isIntegerBetween 0 60
    , regex " ?წუთში"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:_) ->
        tt . inDuration . duration TG.Minute $ floor v
      _ -> Nothing
  }

ruleDurationAfterBeforeTime :: Rule
ruleDurationAfterBeforeTime = Rule
  { name = "<duration> after|before|from <time>"
  , pattern =
    [ dimension Time
    , dimension Duration
    , regex "(წინ)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td: 
       Token Duration dd:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
         "წინ" -> tt $ durationBefore dd td
         _        -> tt $ durationAfter dd td
      _ -> Nothing
  }

ruleDurationAfterBeforeTime1 :: Rule
ruleDurationAfterBeforeTime1 = Rule
  { name = "<duration> after|before|from <time>"
  , pattern =
    [ dimension Duration
    , regex "(წინ)"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
         "წინ" -> tt $ durationBefore dd now
         _        -> tt $ durationAfter dd now
      _ -> Nothing
  }

ruleDurationLastNext1 :: Rule
ruleDurationLastNext1 = Rule
  { name = "last|past|next <duration>"
  , pattern =
    [ regex "ბოლო|უკანასკნელი?|გასული?"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token Duration dd:
       _) -> do
        Token Time <$> interval TTime.Closed (durationAgo dd) now
      _ -> Nothing
  } 

-- For example, Today(დღეს), This year(წელს)
ruleGrainOfTime :: Rule
ruleGrainOfTime = Rule
  { name = "<TimeGrain>"
  , pattern =
    [ Predicate $ or . sequence [isGrainOfTime TG.Week, isGrainOfTime TG.Month, isGrainOfTime TG.Quarter, isGrainOfTime TG.Year]
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:
       _) -> do
        Token Time <$> interval TTime.Closed (cycleNth grain 0) now
      _ -> Nothing
  } 

ruleYear :: Rule
ruleYear = Rule
  { name = "this|last|next year"
  , pattern =
    [ regex "(შარშან|შარშან ?წინ|შარშან ?წინის? ?წინ|გასული? წელი?ს?)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        case Text.toLower match of
          "შარშან"          -> Token Time <$> interval TTime.Open (cycleNth TG.Year (-1)) (cycleNth TG.Year 0)
          "გასული წელი"          -> Token Time <$> interval TTime.Open (cycleNth TG.Year (-1)) (cycleNth TG.Year 0)
          "გასულ წელს"          -> Token Time <$> interval TTime.Open (cycleNth TG.Year (-1)) (cycleNth TG.Year 0)
          "შარშან წინ"        -> Token Time <$> interval TTime.Open (cycleNth TG.Year (-2)) (cycleNth TG.Year (-1))
          "შარშანწინ"       -> Token Time <$> interval TTime.Open (cycleNth TG.Year (-2)) (cycleNth TG.Year (-1))
          "შარშან წინის წინ"          -> Token Time <$> interval TTime.Open (cycleNth TG.Year (-3)) (cycleNth TG.Year (-2))
          "შარშანწინის წინ"          -> Token Time <$> interval TTime.Open (cycleNth TG.Year (-3)) (cycleNth TG.Year (-2))
          "შარშან წინისწინ"          -> Token Time <$> interval TTime.Open (cycleNth TG.Year (-3)) (cycleNth TG.Year (-2))
          "შარშანწინისწინ"          -> Token Time <$> interval TTime.Open (cycleNth TG.Year (-3)) (cycleNth TG.Year (-2))
          _ -> Nothing
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleIntersect
  , ruleDurationLastNext1
  , ruleAbsorbInMonthYear
  , ruleNextDOW
  , ruleNextTime
  , ruleThisTime
  , ruleLastTime
  , ruleTimeAfterNext
  , ruleTimeBeforeLast
  , ruleLastWeekendOfMonth
  , ruleLastWeekendOfMonth1
  , ruleLastWorkweekOfMonth
  , ruleLastWorkweekOfMonth1
  , ruleNthTimeOfTime
  , ruleNthTimeAfterTime
  , ruleYearLatent
  , ruleTheDOMNumeral
  , ruleMonthDOMNumeral1
  , ruleTODLatent
  , ruleAtTOD
  , ruleTODOClock
  , ruleHHMM
  , ruleHHMMLatent
  , ruleHHMMSS
  , ruleMilitaryAMPM
  , ruleMilitarySpelledOutAMPM
  , ruleMilitarySpelledOutAMPM2
  , ruleTODAMPM
  , ruleNumeralToHOD
  , ruleHalfToHOD
  , ruleNumeralAfterHOD
  , ruleNumeralAfterHOD1
  , ruleNumeralAfterHOD2
  , ruleYYYYMMDD
  , ruleMMYYYY
  , rulePartOfDays
  , ruleEarlyMorning
  , ruleTonight
  , ruleAfterPartofday
  , ruleTimePOD
  , rulePODofTime
  , ruleWeekend
  , ruleTODPrecision
  , rulePrecisionTOD
  , ruleIntervalMonthDDDD
  , ruleIntervalMonthDDDD1
  , ruleIntervalDDDDMonth
  , ruleIntervalDDDDMonth1
  , ruleIntervalBetween
  , ruleIntervalBetween1
  , ruleIntervalBetween2
  , ruleIntervalTODDash
  , ruleIntervalTODBetween
  , ruleCycleTheAfterBeforeTime1
  , ruleCycleThisLastNext
  , ruleDOMOfTimeMonth
  , ruleCycleOrdinalOfTime
  , ruleCycleLastOrdinalOfTime
  , ruleDurationInWithinAfter
  , ruleNDOWago
  , ruleDurationHenceAgo
  , ruleDayDurationHenceAgo
  , ruleDurationAfterBeforeTime
  , ruleDurationAfterBeforeTime1
  , ruleInNumeral
  , rulePartOfMonth
  , rulePartOfMonth1
  , ruleEndOrBeginningOfMonth
  , ruleEndOrBeginningOfYear
  , ruleEndOrBeginningOfWeek
  , ruleNow
  , ruleSeason
  , ruleEndOfMonth
  , ruleBeginningOfMonth
  , ruleEndOfYear
  , ruleBeginningOfYear
  , ruleGrainOfTime
  , ruleYear
  , ruleIntervalBetweenMM
  , ruleworkweek
  , ruleNamedMonth
  ]
  ++ ruleInstants
  ++ ruleDaysOfWeek
  ++ ruleMonths
  ++ ruleSeasons
  ++ ruleComputedHolidays
  ++ rulePeriodicHolidays
