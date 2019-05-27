-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Duckling.Time.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Foldable (find)
import Data.Hashable
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.Tuple.Extra (both)
import GHC.Generics
import Prelude
import TextShow (showt)
import qualified Data.HashMap.Strict as H
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Data.Time.Calendar.WeekDate as Time
import qualified Data.Time.LocalTime.TimeZone.Series as Series

import Duckling.Resolve
import Duckling.TimeGrain.Types (Grain)
import qualified Duckling.TimeGrain.Types as TG

data TimeObject = TimeObject
  { start :: Time.UTCTime
  , grain :: Grain
  , end :: Maybe Time.UTCTime
  } deriving (Eq, Show)

data Form = DayOfWeek
  | TimeOfDay
    { hours :: Maybe Int
    , is12H :: Bool
    }
  | Month { month :: Int }
  | PartOfDay
  deriving (Eq, Generic, Hashable, Show, Ord, NFData)

data IntervalDirection = Before | After
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

data TimeData = TimeData
  { timePred :: Predicate
  , latent :: Bool
  , timeGrain :: Grain -- needed for intersect
  , notImmediate :: Bool
  , form :: Maybe Form
  , direction :: Maybe IntervalDirection
  , okForThisNext :: Bool -- allows specific this+Time
  , holiday :: Maybe Text
  , hasTimezone :: Bool -- hack to prevent double timezone parsing
  }

instance Eq TimeData where
  (==) (TimeData _ l1 g1 n1 f1 d1 _ _ t1) (TimeData _ l2 g2 n2 f2 d2 _ _ t2) =
    l1 == l2 && g1 == g2 && n1 == n2 && f1 == f2 && d1 == d2 && t1 == t2

instance Hashable TimeData where
  hashWithSalt s (TimeData _ latent grain imm form dir _ _ _) = hashWithSalt s
    (0::Int, (latent, grain, imm, form, dir))

instance Ord TimeData where
  compare (TimeData _ l1 g1 n1 f1 d1 _ _ _) (TimeData _ l2 g2 n2 f2 d2 _ _ _) =
    case compare g1 g2 of
      EQ -> case compare f1 f2 of
        EQ -> case compare l1 l2 of
          EQ -> case compare n1 n2 of
            EQ -> compare d1 d2
            z -> z
          z -> z
        z -> z
      z -> z

instance Show TimeData where
  show (TimeData _ latent grain _ form dir _ holiday tz) =
    "TimeData{" ++
    "latent=" ++ show latent ++
    ", grain=" ++ show grain ++
    ", form=" ++ show form ++
    ", direction=" ++ show dir ++
    ", holiday=" ++ show holiday ++
    ", hasTimezone=" ++ show tz ++
    "}"

instance NFData TimeData where
  rnf TimeData{..} = rnf (latent, timeGrain, notImmediate, form, direction)

instance Resolve TimeData where
  type ResolvedValue TimeData = TimeValue
  resolve _ Options {withLatent = False} TimeData {latent = True} = Nothing
  resolve context _ TimeData {timePred, latent, notImmediate, direction, holiday} = do
    value <- case future of
      [] -> listToMaybe past
      ahead:nextAhead:_
        | notImmediate && isJust (timeIntersect ahead refTime) -> Just nextAhead
      ahead:_ -> Just ahead
    values <- Just . take 3 $ if List.null future then past else future
    Just $ case direction of
      Nothing -> (TimeValue (timeValue tzSeries value)
        (map (timeValue tzSeries) values) holiday, latent)
      Just d -> (TimeValue (openInterval tzSeries d value)
        (map (openInterval tzSeries d) values) holiday, latent)
    where
      DucklingTime (Series.ZoneSeriesTime utcTime tzSeries) = referenceTime context
      refTime = TimeObject
        { start = utcTime
        , grain = TG.Second
        , end = Nothing
        }
      tc = TimeContext
        { refTime = refTime
        , tzSeries = tzSeries
        , maxTime = timePlus refTime TG.Year 2000
        , minTime = timePlus refTime TG.Year $ - 2000
        }
      (past, future) = runPredicate timePred refTime tc

timedata' :: TimeData
timedata' = TimeData
  { timePred = mkEmptyPredicate
  , latent = False
  , timeGrain = TG.Second
  , notImmediate = False
  , form = Nothing
  , direction = Nothing
  , okForThisNext = False
  , holiday = Nothing
  , hasTimezone = False
  }

data TimeContext = TimeContext
  { refTime  :: TimeObject
  , tzSeries :: Series.TimeZoneSeries
  , maxTime  :: TimeObject
  , minTime  :: TimeObject
  }

data InstantValue = InstantValue
  { vValue :: Time.ZonedTime
  , vGrain :: Grain
  }
  deriving (Show)

instance Eq InstantValue where
  (==) (InstantValue (Time.ZonedTime lt1 tz1) g1)
       (InstantValue (Time.ZonedTime lt2 tz2) g2) =
    g1 == g2 && lt1 == lt2 && tz1 == tz2

data SingleTimeValue
  = SimpleValue InstantValue
  | IntervalValue (InstantValue, InstantValue)
  | OpenIntervalValue (InstantValue, IntervalDirection)
  deriving (Show, Eq)

data TimeValue = TimeValue SingleTimeValue [SingleTimeValue] (Maybe Text)
  deriving (Show, Eq)

instance ToJSON InstantValue where
  toJSON (InstantValue value grain) = object
    [ "value" .= toRFC3339 value
    , "grain" .= grain
    ]

instance ToJSON SingleTimeValue where
  toJSON (SimpleValue value) = case toJSON value of
    Object o -> Object $ H.insert "type" (toJSON ("value" :: Text)) o
    _ -> Object H.empty
  toJSON (IntervalValue (from, to)) = object
    [ "type" .= ("interval" :: Text)
    , "from" .= toJSON from
    , "to" .= toJSON to
    ]
  toJSON (OpenIntervalValue (instant, Before)) = object
    [ "type" .= ("interval" :: Text)
    , "to" .= toJSON instant
    ]
  toJSON (OpenIntervalValue (instant, After)) = object
    [ "type" .= ("interval" :: Text)
    , "from" .= toJSON instant
    ]

instance ToJSON TimeValue where
  toJSON (TimeValue value values holiday) = case toJSON value of
    Object o ->
      Object $ insertHoliday holiday $ H.insert "values" (toJSON values) o
    _ -> Object H.empty
    where
      insertHoliday :: Maybe Text -> Object -> Object
      insertHoliday Nothing obj = obj
      insertHoliday (Just h) obj = H.insert "holidayBeta" (toJSON h) obj

-- | Return a tuple of (past, future) elements
type SeriesPredicate = TimeObject -> TimeContext -> ([TimeObject], [TimeObject])

data AMPM = AM | PM
  deriving (Eq, Show)

data SeasonName = Spring | Summer | Fall | Winter deriving (Enum,Eq,Ord,Show)

-- | Regular seasons of the Northern Hemisphere.
data Season = Season { startYear :: Integer, seasonName :: SeasonName }
  deriving (Eq,Ord,Show)

newtype NoShow a = NoShow a

instance Show (NoShow a) where
  show _ = "??"

data Predicate
  = SeriesPredicate (NoShow SeriesPredicate)
  | EmptyPredicate
  | TimeDatePredicate -- invariant: at least one of them is Just
    { tdSecond :: Maybe Int
    , tdMinute :: Maybe Int
    , tdHour :: Maybe (Bool, Int)
    , tdAMPM :: Maybe AMPM -- only used if we have an hour
    , tdDayOfTheWeek :: Maybe Int
    , tdDayOfTheMonth :: Maybe Int
    , tdMonth :: Maybe Int
    , tdYear :: Maybe Int
    }
  | IntersectPredicate Predicate Predicate
  | TimeIntervalsPredicate TimeIntervalType Predicate Predicate
  | ReplaceIntersectPredicate Predicate Predicate Predicate
  deriving Show

{-# ANN runPredicate ("HLint: ignore Use foldr1OrError" :: String) #-}
runPredicate :: Predicate -> SeriesPredicate
runPredicate EmptyPredicate{} = \_ _ -> ([], [])
runPredicate (SeriesPredicate (NoShow p)) = p
runPredicate TimeDatePredicate{..}
  -- This should not happen by construction, but if it does then
  -- empty time series should be ok
  | isNothing tdHour && isJust tdAMPM = \_ _ -> ([], [])
runPredicate TimeDatePredicate{..} =
  foldr1 runCompose toCompose
  where
  -- runComposePredicate performs best when the first predicate is of
  -- smaller grain, that's why we order by grain here
  toCompose = catMaybes
    [ runSecondPredicate <$> tdSecond
    , runMinutePredicate <$> tdMinute
    , uncurry (runHourPredicate tdAMPM) <$> tdHour
    , runDayOfTheWeekPredicate <$> tdDayOfTheWeek
    , runDayOfTheMonthPredicate <$> tdDayOfTheMonth
    , runMonthPredicate <$> tdMonth
    , runYearPredicate <$> tdYear
    ]
runPredicate (IntersectPredicate pred1 pred2) =
  runIntersectPredicate pred1 pred2
runPredicate (TimeIntervalsPredicate ty pred1 pred2) =
  runTimeIntervalsPredicate ty pred1 pred2
runPredicate (ReplaceIntersectPredicate pred1 pred2 pred3) =
  runReplaceIntersectPredicate pred1 pred2 pred3

-- Don't use outside this module, use a smart constructor
emptyTimeDatePredicate :: Predicate
emptyTimeDatePredicate =
  TimeDatePredicate Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing

-- Predicate smart constructors

-- For debugging find it useful to make it:
-- mkEmptyPredicate :: HasCallStack => Predicate
-- mkEmptyPredicate = EmptyPredicate callStack
-- This way I can track where EmptyPredicates get created
mkEmptyPredicate :: Predicate
mkEmptyPredicate = EmptyPredicate

mkSeriesPredicate :: SeriesPredicate -> Predicate
mkSeriesPredicate = SeriesPredicate . NoShow

mkSecondPredicate :: Int -> Predicate
mkSecondPredicate n = emptyTimeDatePredicate { tdSecond = Just n }

mkMinutePredicate :: Int -> Predicate
mkMinutePredicate n = emptyTimeDatePredicate { tdMinute = Just n }

mkHourPredicate :: Bool -> Int -> Predicate
mkHourPredicate is12H h = emptyTimeDatePredicate { tdHour = Just (is12H, h) }

mkAMPMPredicate :: AMPM -> Predicate
mkAMPMPredicate ampm = emptyTimeDatePredicate { tdAMPM = Just ampm }

mkDayOfTheWeekPredicate :: Int -> Predicate
mkDayOfTheWeekPredicate n = emptyTimeDatePredicate { tdDayOfTheWeek = Just n }

mkDayOfTheMonthPredicate :: Int -> Predicate
mkDayOfTheMonthPredicate n = emptyTimeDatePredicate { tdDayOfTheMonth = Just n }

mkMonthPredicate :: Int -> Predicate
mkMonthPredicate n = emptyTimeDatePredicate { tdMonth = Just n }

mkYearPredicate :: Int -> Predicate
mkYearPredicate n = emptyTimeDatePredicate { tdYear = Just n }

mkIntersectPredicate :: Predicate -> Predicate -> Predicate
mkIntersectPredicate a@EmptyPredicate{} _ = a
mkIntersectPredicate _ a@EmptyPredicate{} = a
mkIntersectPredicate
  (TimeDatePredicate a1 b1 c1 d1 e1 f1 g1 h1)
  (TimeDatePredicate a2 b2 c2 d2 e2 f2 g2 h2)
  = fromMaybe mkEmptyPredicate
      (TimeDatePredicate <$>
        unify a1 a2 <*>
        unify b1 b2 <*>
        unify c1 c2 <*>
        unify d1 d2 <*>
        unify e1 e2 <*>
        unify f1 f2 <*>
        unify g1 g2 <*>
        unify h1 h2)
  where
  unify Nothing a = Just a
  unify a Nothing = Just a
  unify ma@(Just a) (Just b)
    | a == b = Just ma
    | otherwise = Nothing
mkIntersectPredicate pred1 pred2 = IntersectPredicate pred1 pred2

mkReplaceIntersectPredicate :: Predicate -> Predicate -> Predicate -> Predicate
mkReplaceIntersectPredicate pred1 pred2 pred3 =
  ReplaceIntersectPredicate pred1 pred2 pred3

mkTimeIntervalsPredicate
  :: TimeIntervalType -> Predicate -> Predicate -> Predicate
mkTimeIntervalsPredicate _ a@EmptyPredicate{} _ = a
mkTimeIntervalsPredicate _ _ a@EmptyPredicate{} = a
-- `from (... from a to b ...) to c` and `from c to (... from a to b ...)` don't
-- really have a good interpretation, so abort early
mkTimeIntervalsPredicate _ a b
  | containsTimeIntervalsPredicate a ||
    containsTimeIntervalsPredicate b = mkEmptyPredicate
  -- this is potentially quadratic, but the sizes involved should be small
mkTimeIntervalsPredicate t a b = TimeIntervalsPredicate t a b

containsTimeIntervalsPredicate :: Predicate -> Bool
containsTimeIntervalsPredicate TimeIntervalsPredicate{} = True
containsTimeIntervalsPredicate (IntersectPredicate a b) =
  containsTimeIntervalsPredicate a || containsTimeIntervalsPredicate b
containsTimeIntervalsPredicate _ = False
  -- SeriesPredicate might contain one, but we'll underapproximate for
  -- now

-- Computes the difference of the start time of two `TimeObject`s.
diffStartTime :: TimeObject -> TimeObject -> Time.NominalDiffTime
diffStartTime TimeObject{start = x} TimeObject{start = y} =
  abs (Time.diffUTCTime x y)

isEmptyPredicate :: Predicate -> Bool
isEmptyPredicate EmptyPredicate{} = True
isEmptyPredicate _ = False

seasonStart :: Season -> Time.Day
seasonStart (Season year Spring) = Time.fromGregorian year 3 20
seasonStart (Season year Summer) = Time.fromGregorian year 6 21
seasonStart (Season year Fall) = Time.fromGregorian year 9 23
seasonStart (Season year Winter) = Time.fromGregorian year 12 21

seasonEnd :: Season -> Time.Day
seasonEnd = Time.addDays (-1) . seasonStart . nextSeason

nextSeason :: Season -> Season
nextSeason (Season year Winter) = Season (year+1) Spring
nextSeason (Season year x) = Season year (succ x)

prevSeason :: Season -> Season
prevSeason (Season year Spring) = Season (year-1) Winter
prevSeason (Season year x) = Season year (pred x)

seasonOf :: Time.Day -> Season
seasonOf day = fromMaybe (Season (year-1) Winter) mbSeason
  where
  (year,_,_) = Time.toGregorian day
  mbSeason = find ((<= day) . seasonStart) $
               Season year <$> [Winter,Fall,Summer,Spring]

seasonPredicate :: Predicate
seasonPredicate = mkSeriesPredicate series
  where
  series t = const (past,future)
    where
    day = Time.utctDay (start t)
    (past,future) = both (map toTimeObj) (toZipper day)
    toTimeObj season = TimeObject { start = s, grain = TG.Day, end = Just e }
      where (s,e) = both toMidnight (seasonStart season, seasonEnd season)
    toZipper d = (before, currentAndAfter)
      where
      current = seasonOf d
      currentAndAfter = iterate nextSeason current
      before = iterate prevSeason (prevSeason current)

-- Predicate for weekdays, i.e., Mon to Fri.
weekdayPredicate :: Predicate
weekdayPredicate = mkSeriesPredicate series
  where
  series t = const (past,future)
    where
    day = Time.utctDay (start t)
    (_,_,dayOfWeek) = Time.toWeekDate day
    past = toTimeObj . toMidnight . fst <$>
      iterate prevWeekday (prevWeekday (day,dayOfWeek))
    future = toTimeObj . toMidnight <$>
      if dayOfWeek <= 5 then day:days else days
        where days = fst <$> iterate nextWeekday (nextWeekday (day,dayOfWeek))
    toTimeObj t = TimeObject { start = t, grain = TG.Day, end = Nothing }
    nextWeekday (d,dow)
      | dow < 5 = (Time.addDays 1 d, dow+1)
      | otherwise = (Time.addDays (toInteger $ 8-dow) d, 1)
    prevWeekday (d,dow)
      | dow == 1 = (Time.addDays (-3) d, 5)
      | dow == 7 = (Time.addDays (-2) d, 5)
      | otherwise = (Time.addDays (-1) d, dow-1)

-- Predicate for periodic events with known `given`
periodicPredicate :: TG.Grain -> Int -> TimeObject -> Predicate
periodicPredicate grain delta given = mkSeriesPredicate series
  where
  series t _ = (past', future')
    where
    (past, future) = timeSequence grain delta given
    (past', future') = if timeBefore t given
      then
        let (newer, older) = span (timeBefore t) past
        in (older, reverse newer ++ future)
      else
        let (older, newer) = span (`timeBefore` t) future
        in (reverse older ++ past, newer)

toMidnight :: Time.Day -> Time.UTCTime
toMidnight = flip Time.UTCTime (Time.timeOfDayToTime Time.midnight)

-- Predicate runners

runSecondPredicate :: Int -> SeriesPredicate
runSecondPredicate n = series
  where
  series t _ = timeSequence TG.Minute 1 anchor
    where
      Time.UTCTime _ diffTime = start t
      Time.TimeOfDay _ _ s = Time.timeToTimeOfDay diffTime
      anchor = timePlus (timeRound t TG.Second) TG.Second
        $ mod (toInteger n - floor s :: Integer) 60

runMinutePredicate :: Int -> SeriesPredicate
runMinutePredicate n = series
  where
  series t _ = timeSequence TG.Hour 1 anchor
    where
      Time.UTCTime _ diffTime = start t
      Time.TimeOfDay _ m _ = Time.timeToTimeOfDay diffTime
      rounded = timeRound t TG.Minute
      anchor = timePlus rounded TG.Minute . toInteger $ mod (n - m) 60

runHourPredicate :: Maybe AMPM -> Bool -> Int -> SeriesPredicate
runHourPredicate ampm is12H n = series
  where
  series t _ =
    ( drop 1 $
        iterate (\t -> timePlus t TG.Hour . toInteger $ - step) anchor
    , iterate (\t -> timePlus t TG.Hour $ toInteger step) anchor
    )
    where
      Time.UTCTime _ diffTime = start t
      Time.TimeOfDay h _ _ = Time.timeToTimeOfDay diffTime
      step :: Int
      step = if is12H && n <= 12 && isNothing ampm then 12 else 24
      n' = case ampm of
            Just AM -> n `mod` 12
            Just PM -> (n `mod` 12) + 12
            Nothing -> n
      rounded = timeRound t TG.Hour
      anchor = timePlus rounded TG.Hour . toInteger $ mod (n' - h) step

runAMPMPredicate :: AMPM -> SeriesPredicate
runAMPMPredicate ampm = series
  where
  series t _ = (past, future)
    where
    past = maybeShrinkFirst $
      iterate (\t -> timePlusEnd t TG.Hour . toInteger $ - step) anchor
    future = maybeShrinkFirst $
      iterate (\t -> timePlusEnd t TG.Hour $ toInteger step) anchor
    -- to produce time in the future/past we need to adjust
    -- the start/end of the first interval
    maybeShrinkFirst (a:as) =
      case timeIntersect (t { grain = TG.Day }) a of
        Nothing -> as
        Just ii -> ii:as
    maybeShrinkFirst a = a
    step :: Int
    step = 24
    n = case ampm of
          AM -> 0
          PM -> 12
    rounded = timeRound t TG.Day
    anchorStart = timePlus rounded TG.Hour n
    anchorEnd = timePlus anchorStart TG.Hour 12
    -- an interval of length 12h starting either at 12am or 12pm,
    -- the same day as input time
    anchor = timeInterval Open anchorStart anchorEnd

runDayOfTheWeekPredicate :: Int -> SeriesPredicate
runDayOfTheWeekPredicate n = series
  where
  series t _ = timeSequence TG.Day 7 anchor
    where
      Time.UTCTime day _ = start t
      (_, _, dayOfWeek) = Time.toWeekDate day
      daysUntilNextWeek = toInteger $ mod (n - dayOfWeek) 7
      anchor =
        timePlus (timeRound t TG.Day) TG.Day daysUntilNextWeek

runDayOfTheMonthPredicate :: Int -> SeriesPredicate
runDayOfTheMonthPredicate n = series
  where
  series t _ =
    ( map addDays . filter enoughDays . iterate (addMonth $ - 1) $
        addMonth (- 1) anchor
    , map addDays . filter enoughDays $ iterate (addMonth 1) anchor
    )
    where
      enoughDays :: TimeObject -> Bool
      enoughDays t = let Time.UTCTime day _ = start t
                         (year, month, _) = Time.toGregorian day
                     in n <= Time.gregorianMonthLength year month
      addDays :: TimeObject -> TimeObject
      addDays t = timePlus t TG.Day . toInteger $ n - 1
      addMonth :: Int -> TimeObject -> TimeObject
      addMonth i t = timePlus t TG.Month $ toInteger i
      roundMonth :: TimeObject -> TimeObject
      roundMonth t = timeRound t TG.Month
      rounded = roundMonth t
      Time.UTCTime day _ = start t
      (_, _, dayOfMonth) = Time.toGregorian day
      anchor = if dayOfMonth <= n then rounded else addMonth 1 rounded

runMonthPredicate :: Int -> SeriesPredicate
runMonthPredicate n = series
  where
  series t _ = timeSequence TG.Year 1 anchor
    where
      rounded =
        timePlus (timeRound t TG.Year) TG.Month . toInteger $ n - 1
      anchor = if timeStartsBeforeTheEndOf t rounded
        then rounded
        else timePlus rounded TG.Year 1

runYearPredicate :: Int -> SeriesPredicate
runYearPredicate n = series
  where
  series t _ =
    if tyear <= year
      then ([], [y])
      else ([y], [])
    where
      Time.UTCTime day _ = start t
      (tyear, _, _) = Time.toGregorian day
      year = toInteger n
      y = timePlus (timeRound t TG.Year) TG.Year $ year - tyear

-- Limits how deep into lists of segments to look
safeMax :: Int
safeMax = 10

runReplaceIntersectPredicate
  :: Predicate -> Predicate -> Predicate -> SeriesPredicate
runReplaceIntersectPredicate pred1 pred2 pred3 = runComposeWithReplacement
  (runPredicate pred1) (runPredicate pred2) (runPredicate pred3)

-- If pred1 intersects with pred2, returns pred3 otherwise pred2
-- Caveat: only works if all predicates are aligned (e.g. once a year)
runComposeWithReplacement
  :: SeriesPredicate -> SeriesPredicate -> SeriesPredicate -> SeriesPredicate
runComposeWithReplacement pred1 pred2 pred3 = series
  where
  series nowTime context = (backward, forward)
    where
    (past1, future1) = pred1 nowTime context
    (past2, future2) = pred2 nowTime context
    (past3, future3) = pred3 nowTime context

    computeSerie :: [[TimeObject]] -> [TimeObject]
    computeSerie [tokens1,tokens2,tokens3] =
      zipWith3 (\token1 token2 token3 -> case timeIntersect token1 token2 of
        Just _ -> token3
        Nothing -> token2
      ) tokens1 tokens2 tokens3
    computeSerie _ = []

    backwardBounded =
      takeWhile (\t -> timeStartsBeforeTheEndOf (minTime context) t)
      . take safeMax
    forwardBounded =
      takeWhile (\t -> timeStartsBeforeTheEndOf t (maxTime context))
      . take safeMax

    backward = computeSerie $ map backwardBounded [past1, past2, past3]
    forward = computeSerie $ map forwardBounded [future1, future2, future3]

runIntersectPredicate :: Predicate -> Predicate -> SeriesPredicate
runIntersectPredicate pred1 pred2 =
  runCompose (runPredicate pred1) (runPredicate pred2)

-- Performs best when pred1 is smaller grain than pred2
runCompose :: SeriesPredicate -> SeriesPredicate -> SeriesPredicate
runCompose pred1 pred2 = series
  where
  series nowTime context = (backward, forward)
    where
    (past, future) = pred2 nowTime context
    computeSerie tokens =
      [t | time1 <- take safeMax tokens
         , t <- mapMaybe (timeIntersect time1) .
                takeWhile (startsBefore time1) .
                snd . pred1 time1 $ fixedRange time1
      ]

    startsBefore t1 this = timeStartsBeforeTheEndOf this t1
    fixedRange t1 = context {minTime = t1, maxTime = t1}

    backward = computeSerie $ takeWhile (\t ->
      timeStartsBeforeTheEndOf (minTime context) t) past
    forward = computeSerie $ takeWhile (\t ->
      timeStartsBeforeTheEndOf t (maxTime context)) future

runTimeIntervalsPredicate
  :: TimeIntervalType -> Predicate
  -> Predicate -> SeriesPredicate
runTimeIntervalsPredicate intervalType pred1 pred2 = timeSeqMap True f pred1
  where
    -- Pick the first interval *after* the given time segment
    f thisSegment ctx = case runPredicate pred2 thisSegment ctx of
      (_, firstFuture:_) -> Just $
        timeInterval intervalType thisSegment firstFuture
      _ -> Nothing

-- Limits how deep into lists of segments to look
safeMaxInterval :: Int
safeMaxInterval = 12

-- | Applies `f` to each interval yielded by `g`.
-- | Intervals including "now" are in the future.
timeSeqMap
  :: Bool
     -- Given an interval and range, compute a single new interval
  -> (TimeObject -> TimeContext -> Maybe TimeObject)
     -- First-layer series generator
  -> Predicate
     -- Series generator for values that come from `f`
  -> SeriesPredicate
timeSeqMap dontReverse f g = series
  where
  series nowTime context = (past, future)
    where
    -- computes a single interval from `f` based on each interval in the series
    applyF series = mapMaybe (\x -> f x context) $ take safeMaxInterval series

    (firstPast, firstFuture) = runPredicate g nowTime context
    (past1, future1) = (applyF firstPast, applyF firstFuture)

    -- Separate what's before and after now from the past's series
    (newFuture, stillPast) =
      span (timeStartsBeforeTheEndOf nowTime) past1
    -- A series that ends at the earliest time
    oldPast = takeWhile
      (timeStartsBeforeTheEndOf $ minTime context)
      stillPast

    -- Separate what's before and after now from the future's series
    (newPast, stillFuture) =
      break (timeStartsBeforeTheEndOf nowTime) future1
    -- A series that ends at the furthest future time
    oldFuture = takeWhile
      (\x -> timeStartsBeforeTheEndOf x $ maxTime context)
      stillFuture

    -- Reverse the list if needed?
    applyRev series = if dontReverse then series else reverse series
    (sortedPast, sortedFuture) = (applyRev newPast, applyRev newFuture)

    -- Past is the past from the future's series with the
    -- past from the past's series tacked on
    past = sortedPast ++ oldPast

    -- Future is the future from the past's series with the
    -- future from the future's series tacked on
    future = sortedFuture ++ oldFuture


timeSequence
  :: TG.Grain
  -> Int
  -> TimeObject
  -> ([TimeObject], [TimeObject])
timeSequence grain step anchor =
  ( drop 1 $ iterate (f $ - step) anchor
  , iterate (f step) anchor
  )
  where
    f :: Int -> TimeObject -> TimeObject
    f n t = timePlus t grain $ toInteger n

-- | Zero-pad `x` to reach length `n`.
pad :: Int -> Int -> Text
pad n x
  | x <= magnitude = Text.replicate (n - Text.length s) "0" <> s
  | otherwise      = s
  where
    magnitude = round ((10 :: Float) ** fromIntegral (n - 1) :: Float)
    s = showt x

-- | Return the timezone offset portion of the RFC3339 format, e.g. "-02:00".
timezoneOffset :: Time.TimeZone -> Text
timezoneOffset (Time.TimeZone t _ _) = Text.concat [sign, hh, ":", mm]
  where
    (sign, t') = if t < 0 then ("-", negate t) else ("+", t)
    (hh, mm) = both (pad 2) $ divMod t' 60

-- | Return a RFC3339 formatted time, e.g. "2013-02-12T04:30:00.000-02:00".
-- | Backward-compatible with Duckling: fraction of second is milli and padded.
toRFC3339 :: Time.ZonedTime -> Text
toRFC3339 (Time.ZonedTime (Time.LocalTime day (Time.TimeOfDay h m s)) tz) =
  Text.concat
    [ Text.pack $ Time.showGregorian day
    , "T"
    , pad 2 h
    , ":"
    , pad 2 m
    , ":"
    , pad 2 $ floor s
    , "."
    , pad 3 . round $ (s - realToFrac (floor s :: Integer)) * 1000
    , timezoneOffset tz
    ]

instantValue :: Series.TimeZoneSeries -> Time.UTCTime -> Grain -> InstantValue
instantValue tzSeries t g = InstantValue
  { vValue = fromUTC t $ Series.timeZoneFromSeries tzSeries t
  , vGrain = g
  }

timeValue :: Series.TimeZoneSeries -> TimeObject -> SingleTimeValue
timeValue tzSeries (TimeObject s g Nothing) =
  SimpleValue $ instantValue tzSeries s g
timeValue tzSeries (TimeObject s g (Just e)) = IntervalValue
  ( instantValue tzSeries s g
  , instantValue tzSeries e g
  )

openInterval
  :: Series.TimeZoneSeries -> IntervalDirection -> TimeObject -> SingleTimeValue
openInterval tzSeries direction (TimeObject s g _) = OpenIntervalValue
  ( instantValue tzSeries s g
  , direction
  )

-- -----------------------------------------------------------------
-- Time object helpers

timeRound :: TimeObject -> TG.Grain -> TimeObject
timeRound t TG.Week = TimeObject {start = s, grain = TG.Week, end = Nothing}
  where
    Time.UTCTime day diffTime = start $ timeRound t TG.Day
    (year, week, _) = Time.toWeekDate day
    newDay = Time.fromWeekDate year week 1
    s = Time.UTCTime newDay diffTime
timeRound t TG.Quarter = newTime {grain = TG.Quarter}
  where
    monthTime = timeRound t TG.Month
    Time.UTCTime day _ = start monthTime
    (_, month, _) = Time.toGregorian day
    newTime = timePlus monthTime TG.Month . toInteger $ - (mod (month - 1) 3)
timeRound t grain = TimeObject {start = s, grain = grain, end = Nothing}
  where
    Time.UTCTime day diffTime = start t
    timeOfDay = Time.timeToTimeOfDay diffTime
    (year, month, dayOfMonth) = Time.toGregorian day
    Time.TimeOfDay hours mins secs = timeOfDay
    newMonth = if grain > TG.Month then 1 else month
    newDayOfMonth = if grain > TG.Day then 1 else dayOfMonth
    newDay = Time.fromGregorian year newMonth newDayOfMonth
    newHours = if grain > TG.Hour then 0 else hours
    newMins = if grain > TG.Minute then 0 else mins
    newSecs = if grain > TG.Second then 0 else secs
    newDiffTime = Time.timeOfDayToTime $ Time.TimeOfDay newHours newMins newSecs
    s = Time.UTCTime newDay newDiffTime

timePlus :: TimeObject -> TG.Grain -> Integer -> TimeObject
timePlus (TimeObject start grain _) theGrain n = TimeObject
  { start = TG.add start theGrain n
  , grain = min grain theGrain
  , end = Nothing
  }

-- | Shifts the whole interval by n units of theGrain
-- Returned interval has the same length as the input one
timePlusEnd :: TimeObject -> TG.Grain -> Integer -> TimeObject
timePlusEnd (TimeObject start grain end) theGrain n = TimeObject
  { start = TG.add start theGrain n
  , grain = min grain theGrain
  , end = TG.add <$> end <*> return theGrain <*> return n
  }

timeEnd :: TimeObject -> Time.UTCTime
timeEnd (TimeObject start grain end) = fromMaybe (TG.add start grain 1) end

timeStartingAtTheEndOf :: TimeObject -> TimeObject
timeStartingAtTheEndOf t = TimeObject
  {start = timeEnd t, end = Nothing, grain = grain t}

-- | Closed if the interval between A and B should include B
-- Open if the interval should end right before B
data TimeIntervalType = Open | Closed
  deriving (Eq, Show)

timeInterval :: TimeIntervalType -> TimeObject -> TimeObject -> TimeObject
timeInterval
  intervalType
  TimeObject{start = s1, grain = g1}
  TimeObject{start = s2, end = e2, grain = g2} = TimeObject
  { start = s1
  , grain = g'
  , end = Just $ case intervalType of
      Open -> s2
      Closed -> fromMaybe (TG.add s2 g2' 1) e2
  }
  where
    g' = min g1 g2
    g2'
      | g1 < TG.Day && g2 < TG.Day = g'
      | otherwise = g2

timeStartsBeforeTheEndOf :: TimeObject -> TimeObject -> Bool
timeStartsBeforeTheEndOf t1 t2 = start t1 < timeEnd t2

timeBefore :: TimeObject -> TimeObject -> Bool
timeBefore t1 t2 = start t1 < start t2

-- | Intersection between two `TimeObject`.
-- The resulting grain and end fields are the smallest.
-- Prefers intervals when the range is equal.
timeIntersect :: TimeObject -> TimeObject -> Maybe TimeObject
timeIntersect t1 t2
  | s1 > s2 = timeIntersect t2 t1
  | e1 <= s2 = Nothing
  | e1 < e2 || s1 == s2 && e1 == e2 && isJust end1 = Just TimeObject
    {start = s2, end = end1, grain = g'}
  | otherwise = Just t2 {grain = g'}
  where
    TimeObject s1 g1 end1 = t1
    TimeObject s2 g2 _    = t2
    e1 = timeEnd t1
    e2 = timeEnd t2
    g' = min g1 g2
