-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}


module Duckling.Time.Helpers
  ( -- Patterns
    hasNoDirection, isADayOfWeek, isAMonth, isAnHourOfDay, isAPartOfDay
  , isATimeOfDay, isDOMInteger, isDOMOrdinal, isDOMValue, isGrain
  , isGrainFinerThan, isGrainOfTime, isIntegerBetween, isNotLatent
  , isOrdinalBetween, isMidnightOrNoon, isOkWithThisNext
    -- Production
  , cycleLastOf, cycleN, cycleNth, cycleNthAfter, dayOfMonth, dayOfWeek
  , durationAfter, durationAgo, durationBefore, mkOkForThisNext, form, hour
  , hourMinute, hourMinuteSecond, inDuration, intersect, intersectDOM, interval
  , inTimezone, longWEBefore, minute, minutesAfter, minutesBefore, mkLatent
  , month, monthDay, notLatent, now, nthDOWOfMonth, partOfDay, predLastOf
  , predNth, predNthAfter, second, timeOfDayAMPM, weekend, withDirection, year
  , yearMonthDay, tt
    -- Other
  , getIntValue, timeComputed
  -- Rule constructors
  , mkRuleInstants, mkRuleDaysOfWeek, mkRuleMonths, mkRuleSeasons
  , mkRuleHolidays
  ) where

import Data.Maybe
import Data.Text (Text)
import Prelude
import qualified Data.Time as Time
import qualified Data.Time.Calendar.WeekDate as Time
import qualified Data.Time.LocalTime.TimeZone.Series as Series

import Duckling.Dimensions.Types
import Duckling.Duration.Types (DurationData (DurationData))
import Duckling.Ordinal.Types (OrdinalData (OrdinalData))
import Duckling.Time.TimeZone.Parse (parseTimezone)
import Duckling.Time.Types
  ( TimeData(TimeData)
  , mkSeriesPredicate
  , mkSecondPredicate
  , mkMinutePredicate
  , mkHourPredicate
  , mkAMPMPredicate
  , mkMonthPredicate
  , mkDayOfTheWeekPredicate
  , mkDayOfTheMonthPredicate
  , mkYearPredicate
  , mkIntersectPredicate
  , mkTimeIntervalsPredicate
  , runPredicate
  , AMPM(..)
  )
import Duckling.Types
import qualified Duckling.Duration.Types as TDuration
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.Ordinal.Types as TOrdinal
import qualified Duckling.Time.Types as TTime
import qualified Duckling.TimeGrain.Types as TG

getIntValue :: Token -> Maybe Int
getIntValue (Token Numeral nd) = TNumeral.getIntValue $ TNumeral.value nd
getIntValue (Token Ordinal OrdinalData {TOrdinal.value = x}) = Just x
getIntValue _ = Nothing

timeNegPeriod :: DurationData -> DurationData
timeNegPeriod (DurationData v g) = DurationData
  {TDuration.grain = g, TDuration.value = negate v}

-- -----------------------------------------------------------------
-- Time predicates

timeComputed :: [TTime.TimeObject] -> TTime.Predicate
timeComputed xs = mkSeriesPredicate series
  where
    series t _ = span (flip TTime.timeBefore t) xs

timeCycle :: TG.Grain -> TTime.Predicate
timeCycle grain = mkSeriesPredicate series
  where
  series t _ = TTime.timeSequence grain 1 $ TTime.timeRound t grain

timeSecond :: Int -> TTime.Predicate
timeSecond n = mkSecondPredicate n

timeMinute :: Int -> TTime.Predicate
timeMinute n = mkMinutePredicate n

timeHour :: Bool -> Int -> TTime.Predicate
timeHour is12H n = mkHourPredicate is12H n

timeDayOfWeek :: Int -> TTime.Predicate
timeDayOfWeek n = mkDayOfTheWeekPredicate n

timeDayOfMonth :: Int -> TTime.Predicate
timeDayOfMonth n = mkDayOfTheMonthPredicate n

timeMonth :: Int -> TTime.Predicate
timeMonth n = mkMonthPredicate n

-- | Converts 2-digits to a year between 1950 and 2050
timeYear :: Int -> TTime.Predicate
timeYear n = mkYearPredicate n

-- | Takes `n` cycles of `f`
takeN :: Int -> Bool -> TTime.Predicate -> TTime.Predicate
takeN n notImmediate f = mkSeriesPredicate series
  where
  series t context =
    case slot of
      Just nth -> if TTime.timeStartsBeforeTheEndOf t nth
        then ([], [nth])
        else ([nth], [])
      Nothing -> ([], [])
    where
      baseTime = TTime.refTime context
      (past, future) = runPredicate f baseTime context
      fut = case future of
        (ahead:rest)
          | notImmediate && isJust (TTime.timeIntersect ahead baseTime) -> rest
        _ -> future
      slot = if n >= 0
        then case fut of
          (start:_) -> case drop n fut of
            (end:_) -> Just $ TTime.timeInterval TTime.Open start end
            _ -> Nothing
          _ -> Nothing
        else case past of
          (end:_) -> case drop ((- n) - 1) past of
            (start:_) -> Just $ TTime.timeInterval TTime.Closed start end
            _ -> Nothing
          _ -> Nothing

-- | -1 is the first element in the past
-- | 0 is the first element in the future
takeNth :: Int -> Bool -> TTime.Predicate -> TTime.Predicate
takeNth n notImmediate f = mkSeriesPredicate series
  where
  series t context =
    case rest of
      [] -> ([], [])
      (nth:_) -> if TTime.timeStartsBeforeTheEndOf t nth
        then ([], [nth])
        else ([nth], [])
    where
      baseTime = TTime.refTime context
      (past, future) = runPredicate f baseTime context
      rest = if n >= 0
        then case future of
          (ahead:_) | notImmediate && isJust (TTime.timeIntersect ahead baseTime)
            -> drop (n + 1) future
          _ -> drop n future
        else drop (- (n + 1)) past

-- | Like `takeNth`, but takes the nth cyclic predicate after `basePred`
takeNthAfter
  :: Int
  -> Bool
  -> TTime.Predicate
  -> TTime.Predicate
  -> TTime.Predicate
takeNthAfter n notImmediate cyclicPred basePred =
  mkSeriesPredicate $! TTime.timeSeqMap False f basePred
  where
    f t ctx =
      let (past, future) = runPredicate cyclicPred t ctx
          rest = if n >= 0
                   then case future of
                     (ahead:_) | notImmediate && TTime.timeBefore ahead t
                       -> drop (n + 1) future
                     _ -> drop n future
                   else drop (- (n + 1)) past
      in case rest of
           [] -> Nothing
           (nth:_) -> Just nth

-- | Takes the last occurrence of `cyclicPred` within `basePred`.
takeLastOf :: TTime.Predicate -> TTime.Predicate -> TTime.Predicate
takeLastOf cyclicPred basePred =
  mkSeriesPredicate $! TTime.timeSeqMap False f basePred
  where
    f :: TTime.TimeObject -> TTime.TimeContext -> Maybe TTime.TimeObject
    f t ctx =
      case runPredicate cyclicPred (TTime.timeStartingAtTheEndOf t) ctx of
        (nth:_, _) -> Just nth
        _ -> Nothing

-- | Assumes the grain of `pred1` is smaller than the one of `pred2`
timeCompose :: TTime.Predicate -> TTime.Predicate -> TTime.Predicate
timeCompose pred1 pred2 = mkIntersectPredicate pred1 pred2

addDuration :: DurationData -> TTime.TimeObject -> TTime.TimeObject
addDuration (DurationData n g) t = TTime.timePlus t g $ toInteger n

mergeDuration :: TTime.Predicate -> DurationData -> TTime.Predicate
mergeDuration pred1 dd@(DurationData _ g) =
  mkSeriesPredicate $! TTime.timeSeqMap False f pred1
  where
    f x@TTime.TimeObject{TTime.grain = tg} _ = Just $ addDuration dd t'
      where
        g' = min tg g
        t' = if g' == tg then x else TTime.timeRound x g'

shiftDuration :: TTime.Predicate -> DurationData -> TTime.Predicate
shiftDuration pred1 dd@(DurationData _ g) =
  mkSeriesPredicate $! TTime.timeSeqMap False f pred1
  where
    f x _ = Just . addDuration dd . TTime.timeRound x $ TG.lower g

shiftTimezone :: Series.TimeZoneSeries -> TTime.Predicate -> TTime.Predicate
shiftTimezone providedSeries pred1 =
  mkSeriesPredicate $! TTime.timeSeqMap False f pred1
  where
    f x@(TTime.TimeObject s _ _) ctx =
      let Time.TimeZone ctxOffset _ _ =
            Series.timeZoneFromSeries (TTime.tzSeries ctx) s
          Time.TimeZone providedOffset _ _ =
            Series.timeZoneFromSeries providedSeries s
      -- This forgets about TTime.end, but it's OK since we act on time-of-days.
      in Just . TTime.timePlus x TG.Minute . toInteger $
           ctxOffset - providedOffset

-- -----------------------------------------------------------------
-- Patterns

isGrain :: TG.Grain -> Predicate
isGrain value (Token TimeGrain grain) = grain == value
isGrain _ _ = False

isGrainFinerThan :: TG.Grain -> Predicate
isGrainFinerThan value (Token Time TimeData{TTime.timeGrain = g}) = g < value
isGrainFinerThan _ _ = False

isGrainOfTime :: TG.Grain -> Predicate
isGrainOfTime value (Token Time TimeData{TTime.timeGrain = g}) = g == value
isGrainOfTime _ _ = False

isADayOfWeek :: Predicate
isADayOfWeek (Token Time td) = case TTime.form td of
  Just TTime.DayOfWeek -> True
  _ -> False
isADayOfWeek _ = False

isATimeOfDay :: Predicate
isATimeOfDay (Token Time td) = case TTime.form td of
  Just (TTime.TimeOfDay _ _) -> True
  _ -> False
isATimeOfDay _ = False

isAPartOfDay :: Predicate
isAPartOfDay (Token Time td) = case TTime.form td of
  Just TTime.PartOfDay -> True
  _ -> False
isAPartOfDay _ = False

isAMonth :: Predicate
isAMonth (Token Time td) = case TTime.form td of
  Just (TTime.Month _) -> True
  _ -> False
isAMonth _ = False

isAnHourOfDay :: Predicate
isAnHourOfDay (Token Time td) = case TTime.form td of
  Just (TTime.TimeOfDay (Just _) _) | TTime.timeGrain td > TG.Minute -> True
  _ -> False
isAnHourOfDay _ = False

isMidnightOrNoon :: Predicate
isMidnightOrNoon (Token Time td) = case TTime.form td of
  Just (TTime.TimeOfDay (Just x) _) -> x == 0 || x == 12
  _ -> False
isMidnightOrNoon _ = False

isNotLatent :: Predicate
isNotLatent (Token Time td) = not $ TTime.latent td
isNotLatent _ = False

hasNoDirection :: Predicate
hasNoDirection (Token Time td) = isNothing $ TTime.direction td
hasNoDirection _ = False

isIntegerBetween :: Int -> Int -> Predicate
isIntegerBetween low high (Token Numeral nd) = TNumeral.okForAnyTime nd
  && TNumeral.isIntegerBetween (TNumeral.value nd) low high
isIntegerBetween _ _ _ = False

isOrdinalBetween :: Int -> Int -> Predicate
isOrdinalBetween low high (Token Ordinal od) =
  TOrdinal.isBetween (TOrdinal.value od) low high
isOrdinalBetween _ _ _ = False

isDOMOrdinal :: Predicate
isDOMOrdinal = isOrdinalBetween 1 31

isDOMInteger :: Predicate
isDOMInteger = isIntegerBetween 1 31

isDOMValue :: Predicate
isDOMValue = or . sequence [isDOMOrdinal, isDOMInteger]

isOkWithThisNext :: Predicate
isOkWithThisNext (Token Time TimeData {TTime.okForThisNext = True}) = True
isOkWithThisNext _ = False

-- -----------------------------------------------------------------
-- Production

-- Pass the interval second
intersect :: TimeData -> TimeData -> Maybe TimeData
intersect td1 td2 =
  case intersect' (td1, td2) of
    TTime.TimeData { TTime.timePred = pred }
      | TTime.isEmptyPredicate pred -> Nothing
    res -> Just res

intersect' :: (TimeData, TimeData) -> TimeData
intersect' (TimeData pred1 _ g1 _ _ d1 _, TimeData pred2 _ g2 _ _ d2 _)
  | g1 < g2 = TTime.timedata'
    { TTime.timePred = timeCompose pred1 pred2
    , TTime.timeGrain = g1
    , TTime.direction = dir
    }
  | otherwise = TTime.timedata'
    { TTime.timePred = timeCompose pred2 pred1
    , TTime.timeGrain = g2
    , TTime.direction = dir
    }
  where
    dir = case catMaybes [d1, d2] of
      [] -> Nothing
      (x:_) -> Just x

now :: TimeData
now = td {TTime.timeGrain = TG.NoGrain}
  where
    td = cycleNth TG.Second 0

hour :: Bool -> Int -> TimeData
hour is12H n = timeOfDay (Just n) is12H $ TTime.timedata'
  {TTime.timePred = timeHour is12H n, TTime.timeGrain = TG.Hour}

minute :: Int -> TimeData
minute n = TTime.timedata'
  {TTime.timePred = timeMinute n, TTime.timeGrain = TG.Minute}

second :: Int -> TimeData
second n = TTime.timedata'
  {TTime.timePred = timeSecond n, TTime.timeGrain = TG.Second}

dayOfWeek :: Int -> TimeData
dayOfWeek n = form TTime.DayOfWeek $ TTime.timedata'
  { TTime.timePred = timeDayOfWeek n
  , TTime.timeGrain = TG.Day
  , TTime.notImmediate = True
  }

dayOfMonth :: Int -> TimeData
dayOfMonth n = TTime.timedata'
  {TTime.timePred = timeDayOfMonth n, TTime.timeGrain = TG.Day}

month :: Int -> TimeData
month n = form TTime.Month {TTime.month = n} $ TTime.timedata'
  {TTime.timePred = timeMonth n, TTime.timeGrain = TG.Month}

year :: Int -> TimeData
year n = TTime.timedata' {TTime.timePred = timeYear n, TTime.timeGrain = TG.Year}

yearMonthDay :: Int -> Int -> Int -> TimeData
yearMonthDay y m d = intersect' (intersect' (year y, month m), dayOfMonth d)

monthDay :: Int -> Int -> TimeData
monthDay m d = intersect' (month m, dayOfMonth d)

hourMinute :: Bool -> Int -> Int -> TimeData
hourMinute is12H h m = timeOfDay (Just h) is12H $
  intersect' (hour is12H h, minute m)

hourMinuteSecond :: Bool -> Int -> Int -> Int -> TimeData
hourMinuteSecond is12H h m s = timeOfDay (Just h) is12H $
  intersect' (intersect' (hour is12H h, minute m), second s)

cycleN :: Bool -> TG.Grain -> Int -> TimeData
cycleN notImmediate grain n = TTime.timedata'
  { TTime.timePred = takeN n notImmediate $ timeCycle grain
  , TTime.timeGrain = grain
  }

cycleNth :: TG.Grain -> Int -> TimeData
cycleNth grain n = TTime.timedata'
  {TTime.timePred = takeNth n False $ timeCycle grain, TTime.timeGrain = grain}

cycleNthAfter :: Bool -> TG.Grain -> Int -> TimeData -> TimeData
cycleNthAfter notImmediate grain n TimeData {TTime.timePred = p} =
  TTime.timedata'
    { TTime.timePred = takeNthAfter n notImmediate (timeCycle grain) p
    , TTime.timeGrain = grain
    }

cycleLastOf :: TG.Grain -> TimeData -> TimeData
cycleLastOf grain TimeData {TTime.timePred = p} = TTime.timedata'
  { TTime.timePred = takeLastOf (timeCycle grain) p
  , TTime.timeGrain = grain
  }

-- Generalized version of cycleLastOf with custom predicate
predLastOf :: TimeData -> TimeData -> TimeData
predLastOf TimeData {TTime.timePred = cyclicPred, TTime.timeGrain = g} base =
  TTime.timedata'
    { TTime.timePred = takeLastOf cyclicPred $ TTime.timePred base
    , TTime.timeGrain = g
    }

-- Generalized version of cycleNth with custom predicate
predNth :: Int -> Bool -> TimeData -> TimeData
predNth n notImmediate TimeData {TTime.timePred = p, TTime.timeGrain = g} =
  TTime.timedata'
    {TTime.timePred = takeNth n notImmediate p, TTime.timeGrain = g}

-- Generalized version of `cycleNthAfter` with custom predicate
predNthAfter :: Int -> TimeData -> TimeData -> TimeData
predNthAfter n TimeData {TTime.timePred = p, TTime.timeGrain = g} base =
  TTime.timedata'
    { TTime.timePred = takeNthAfter n True p $ TTime.timePred base
    , TTime.timeGrain = g
    }

interval' :: TTime.TimeIntervalType -> (TimeData, TimeData) -> TimeData
interval' intervalType (TimeData p1 _ g1 _ _ _ _, TimeData p2 _ g2 _ _ _ _) =
  TTime.timedata'
    { TTime.timePred = mkTimeIntervalsPredicate intervalType' p1 p2
    , TTime.timeGrain = min g1 g2
    }
    where
      intervalType'
        | g1 == g2 && g1 == TG.Day = TTime.Closed
        | otherwise = intervalType

interval :: TTime.TimeIntervalType -> TimeData -> TimeData -> Maybe TimeData
interval intervalType td1 td2 =
  case interval' intervalType (td1, td2) of
    TTime.TimeData { TTime.timePred = pred }
      | TTime.isEmptyPredicate pred -> Nothing
    res -> Just res

mkOkForThisNext :: TimeData -> TimeData
mkOkForThisNext td = td {TTime.okForThisNext = True}

durationAgo :: DurationData -> TimeData
durationAgo dd = inDuration $ timeNegPeriod dd

durationAfter :: DurationData -> TimeData -> TimeData
durationAfter dd TimeData {TTime.timePred = pred1, TTime.timeGrain = g} =
  TTime.timedata'
    { TTime.timePred = if g == TG.NoGrain
      then shiftDuration pred1 dd
      else mergeDuration pred1 dd
    , TTime.timeGrain = TDuration.grain dd
    }

durationBefore :: DurationData -> TimeData -> TimeData
durationBefore dd pred1 = durationAfter (timeNegPeriod dd) pred1

inDuration :: DurationData -> TimeData
inDuration dd = TTime.timedata'
  { TTime.timePred = shiftDuration t dd
  , TTime.timeGrain = TDuration.grain dd
  }
  where
    t = takeNth 0 False $ timeCycle TG.Second

inTimezone :: Text -> TimeData -> Maybe TimeData
inTimezone input td@TimeData {TTime.timePred = p} = do
  tz <- parseTimezone input
  Just $ td {TTime.timePred = shiftTimezone (Series.TimeZoneSeries tz []) p}

mkLatent :: TimeData -> TimeData
mkLatent td = td {TTime.latent = True}

notLatent :: TimeData -> TimeData
notLatent td = td {TTime.latent = False}

form :: TTime.Form -> TimeData -> TimeData
form f td = td {TTime.form = Just f}

partOfDay :: TimeData -> TimeData
partOfDay td = form TTime.PartOfDay td

timeOfDay :: Maybe Int -> Bool -> TimeData -> TimeData
timeOfDay h is12H = form TTime.TimeOfDay {TTime.hours = h, TTime.is12H = is12H}

timeOfDayAMPM :: Bool -> TimeData -> TimeData
timeOfDayAMPM isAM tod = timeOfDay Nothing False $ intersect' (tod, ampm)
  where
    ampm = TTime.timedata'
           { TTime.timePred = ampmPred
           , TTime.timeGrain = TG.Hour
           }
    ampmPred = if isAM then mkAMPMPredicate AM else mkAMPMPredicate PM

withDirection :: TTime.IntervalDirection -> TimeData -> TimeData
withDirection dir td = td {TTime.direction = Just dir}

longWEBefore :: TimeData -> TimeData
longWEBefore monday = interval' TTime.Open (start, end)
  where
    start = intersect' (fri, hour False 18)
    end = intersect' (tue, hour False 0)
    fri = cycleNthAfter False TG.Day (- 3) monday
    tue = cycleNthAfter False TG.Day 1 monday

weekend :: TimeData
weekend = interval' TTime.Open (fri, mon)
  where
    fri = intersect' (dayOfWeek 5, hour False 18)
    mon = intersect' (dayOfWeek 1, hour False 0)

-- Zero-indexed weeks, Monday is 1
-- Use `predLastOf` for last day of week of month
nthDOWOfMonth :: Int -> Int -> Int -> TimeData
nthDOWOfMonth n dow m = predNthAfter (n - 1) dow_ month_
  where
    dow_ = dayOfWeek dow
    month_ = month m

intersectDOM :: TimeData -> Token -> Maybe TimeData
intersectDOM td token = do
  n <- getIntValue token
  intersect (dayOfMonth n) td

minutesBefore :: Int -> TimeData -> Maybe TimeData
minutesBefore n TimeData {TTime.form = Just (TTime.TimeOfDay (Just 0) is12H)} =
  Just $ hourMinute is12H 23 (60 - n)
minutesBefore n TimeData {TTime.form = Just (TTime.TimeOfDay (Just 1) True)} =
  Just $ hourMinute True 12 (60 - n)
minutesBefore n TimeData {TTime.form = Just (TTime.TimeOfDay (Just h) is12H)} =
  Just $ hourMinute is12H (h - 1) (60 - n)
minutesBefore _ _ = Nothing

minutesAfter :: Int -> TimeData -> Maybe TimeData
minutesAfter n TimeData {TTime.form = Just (TTime.TimeOfDay (Just h) is12H)} =
  Just $ hourMinute is12H h n
minutesAfter _ _ = Nothing

-- | Convenience helper to return a time token from a rule
tt :: TimeData -> Maybe Token
tt = Just . Token Time

-- | Rule constructors
mkSingleRegexRule :: Text -> String -> Maybe Token -> Rule
mkSingleRegexRule name pattern token = Rule
  { name = name
  , pattern = [regex pattern]
  , prod = const token
  }

mkRuleInstants :: [(Text, TG.Grain, Int, String)] -> [Rule]
mkRuleInstants = map go
  where
    go (name, grain, n, ptn) = mkSingleRegexRule name ptn . tt $
      cycleNth grain n

mkRuleDaysOfWeek :: [(Text, String)] -> [Rule]
mkRuleDaysOfWeek daysOfWeek = zipWith go daysOfWeek [1..7]
  where
    go (name, ptn) i =
      mkSingleRegexRule name ptn . tt . mkOkForThisNext $ dayOfWeek i

mkRuleMonths :: [(Text, String)] -> [Rule]
mkRuleMonths months  = zipWith go months [1..12]
  where
    go (name, ptn) i =
      mkSingleRegexRule name ptn . tt . mkOkForThisNext $ month i

mkRuleSeasons :: [(Text, String, TimeData, TimeData)] -> [Rule]
mkRuleSeasons = map go
  where
    go (name, ptn, start, end) = mkSingleRegexRule name ptn $
      Token Time <$> mkOkForThisNext <$> interval TTime.Open start end

mkRuleHolidays :: [(Text, String, TimeData)] -> [Rule]
mkRuleHolidays = map go
  where
    go (name, ptn, td) = mkSingleRegexRule name ptn . tt $ mkOkForThisNext td
