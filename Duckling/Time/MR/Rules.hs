{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.MR.Rules where

import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers (duration)
import Duckling.Duration.Types (DurationData (..))
import Duckling.Numeral.Helpers (parseInt, isNatural)
import qualified Duckling.Numeral.Types as TNumeral
import Duckling.Regex.Types
import Duckling.Time.Helpers
import Duckling.Time.Types (TimeData (..))
import Duckling.Types
import qualified Duckling.Ordinal.Types as TOrdinal
import qualified Duckling.Time.Types as TTime
import qualified Duckling.TimeGrain.Types as TG

ruleToday :: Rule
ruleToday = Rule
  { name = "today"
  , pattern =
    [ regex "आजच|आज"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 0
  }

ruleTomorrow :: Rule
ruleTomorrow = Rule
  { name = "tomorrow"
  , pattern =
    [ regex "उद्या"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 1
  }

ruleDayAfterTomorrow :: Rule
ruleDayAfterTomorrow = Rule
  { name = "day after tomorrow"
  , pattern =
    [ regex "परवा"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 2
  }

ruleEveryCycle :: Rule
ruleEveryCycle = Rule
  { name = "every <cycle>"
  , pattern =
    [ regex "दर|प्रत्येक"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) -> tt $ (cycleNth grain 1) {TTime.latent = True}
      _ -> Nothing
  }

ruleNextMonth :: Rule
ruleNextMonth = Rule
  { name = "next month"
  , pattern =
    [ regex "येत्या|पुढील|पुढच्या|पुढचा|पुढची|येणारा|आगामी"
    , regex "महिन्यात|महिन्या|महिने|महिना|मास"
    ]
  , prod = \_ -> tt $ cycleNth TG.Month 1
  }

ruleNextDOW :: Rule
ruleNextDOW = Rule
  { name = "next <day-of-week>"
  , pattern =
    [ regex "येत्या|पुढील|पुढच्या|पुढचा|पुढची|येणारा|आगामी"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time dow:_) -> tt $ predNth 0 True dow
      _ -> Nothing
  }

ruleLastDOW :: Rule
ruleLastDOW = Rule
  { name = "last <day-of-week>"
  , pattern =
    [ regex "गेल्या|मागील|मागच्या|मागचा|मागची"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time dow:_) -> tt $ predNth (-1) False dow
      _ -> Nothing
  }

ruleThisDOW :: Rule
ruleThisDOW = Rule
  { name = "this <day-of-week>"
  , pattern =
    [ regex "या|ह्या"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time dow:_) -> tt $ predNth 0 False dow
      _ -> Nothing
  }

ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = mkRuleDaysOfWeek
  [ ( "सोमवार", "सोमवारापासून|सोमवारी|सोमवारच्या|सोमवारचा|सोमवारची|सोमवारचे|सोमवारा|सोमवार|सोम\\.?" )
  , ( "मंगळवार", "मंगळवारापासून|मंगळवारी|मंगळवारच्या|मंगळवारचा|मंगळवारची|मंगळवारचे|मंगळवारा|मंगळवार|मंगळ\\.?" )
  , ( "बुधवार", "बुधवारापासून|बुधवारी|बुधवारच्या|बुधवारचा|बुधवारची|बुधवारचे|बुधवारा|बुधवार|बुध\\.?" )
  , ( "गुरूवार", "गुरुवारापासून|गुरूवारापासून|गुरुवारी|गुरूवारी|गुरुवारच्या|गुरूवारच्या|गुरुवारचा|गुरूवारचा|गुरुवारची|गुरूवारची|गुरुवारचे|गुरूवारचे|गुरुवारा|गुरूवारा|गुरुवार|गुरूवार|गुरु" )
  , ( "शुक्रवार", "शुक्रवारापासून|शुक्रवारी|शुक्रवारच्या|शुक्रवारचा|शुक्रवारची|शुक्रवारचे|शुक्रवारा|शुक्रवार|शुक्र\\.?" )
  , ( "शनिवार", "शनिवारापासून|शनिवारी|शनिवारच्या|शनिवारचा|शनिवारची|शनिवारचे|शनिवारा|शनिवार|शनि\\.?" )
  , ( "रविवार", "रविवारापासून|रविवारी|रविवारच्या|रविवारचा|रविवारची|रविवारचे|रविवारा|रविवार|रवि" )
  ]

ruleMonths :: [Rule]
ruleMonths = mkRuleMonths
  [ ( "जानेवारी", "जानेवारी(च्या|चे|ची|चा|त|ला|मध्ये)?|जान्यू\\.?" )
  , ( "फेब्रुवारी", "फेब्रुवारी(च्या|चे|ची|चा|त|ला|मध्ये)?|फेब्रू\\.?" )
  , ( "मार्च", "मार्च(च्या|चे|ची|चा|ात|ाला|मध्ये)?" )
  , ( "एप्रिल", "एप्रिल(च्या|चे|ची|चा|ात|ाला|मध्ये)?" )
  , ( "मे", "मे(च्या|चे|ची|चा|मध्ये)?" )
  , ( "जून", "जून(च्या|चे|ची|चा|ात|ाला|मध्ये)?" )
  , ( "जुलै", "जुलै(च्या|चे|ची|चा|त|ला|मध्ये)?" )
  , ( "ऑगस्ट", "ऑगस्ट(च्या|चे|ची|चा|ात|ाला|मध्ये)?|ऑगस्ट" )
  , ( "सप्टेंबर", "सप्टेंबर(च्या|चे|ची|चा|ात|ाला|मध्ये)?|सप्टेंबर|सप्टें\\.?" )
  , ( "ऑक्टोबर", "ऑक्टोबर(च्या|चे|ची|चा|ात|ाला|मध्ये)?|ऑक्टोबर|ऑक्टो\\.?" )
  , ( "नोव्हेंबर", "नोव्हेंबर(च्या|चे|ची|चा|ात|ाला|मध्ये)?|नोव्हेंबर|नोव्हें\\.?" )
  , ( "डिसेंबर", "डिसेंबर(च्या|चे|ची|चा|ात|ाला|मध्ये)?|डिसेंबर|डिसें\\.?" )
  ]

devanagariToArabChar :: Char -> Char
devanagariToArabChar c = case c of
  '०' -> '0'
  '१' -> '1'
  '२' -> '2'
  '३' -> '3'
  '४' -> '4'
  '५' -> '5'
  '६' -> '6'
  '७' -> '7'
  '८' -> '8'
  '९' -> '9'
  _   -> c

ruleNoonMidnight :: Rule
ruleNoonMidnight = Rule
  { name = "noon|midnight"
  , pattern =
    [ regex "(मध्यान्ह|दुपार|मध्यरात्री|मध्यरात्र)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> tt $ hour False $
        if match `elem` ["मध्यान्ह", "दुपार"] then 12 else 0
      _ -> Nothing
  }

ruleHHMM :: Rule
ruleHHMM = Rule
  { name = "hh:mm"
  , pattern = [regex "((?:[01]?\\d)|(?:2[0-3])|(?:[०१]?[०-९])|(?:२[०-३]))[:.]([0-5]\\d|[०-५][०-९])"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (hh:mm:_)):_) -> do
        h <- parseInt $ Text.map devanagariToArabChar hh
        m <- parseInt $ Text.map devanagariToArabChar mm
        tt $ hourMinute (h < 12) h m
      _ -> Nothing
  }

ruleVajta :: Rule
ruleVajta = Rule
  { name = "<time-of-day> वाजता|मिनिटे"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "वाजता|वाजेता|वाजतांना|मिनिटे|मिनिटांनी"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleSampurnaTime :: Rule
ruleSampurnaTime = Rule
  { name = "संपूर्ण|पूर्ण <time>"
  , pattern =
    [ regex "संपूर्ण|पूर्ण"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Just token
      _ -> Nothing
  }

rulePartOfDays :: Rule
rulePartOfDays = Rule
  { name = "part of days"
  , pattern =
    [ regex "(सकाळी|सकाळ|प्रभाती|दुपारी|दुपार|संध्याकाळी|सायंकाळी|संध्याकाळ|सायंकाळ|रात्री|रात्र)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        let (start, end) = case match of
              "सकाळी"    -> (hour False 4, hour False 12)
              "सकाळ"     -> (hour False 4, hour False 12)
              "प्रभाती"   -> (hour False 4, hour False 12)
              "दुपारी"    -> (hour False 12, hour False 16)
              "दुपार"     -> (hour False 12, hour False 16)
              "संध्याकाळी" -> (hour False 16, hour False 20)
              "सायंकाळी"  -> (hour False 16, hour False 20)
              "संध्याकाळ" -> (hour False 16, hour False 20)
              "सायंकाळ"   -> (hour False 16, hour False 20)
              "रात्री"    -> (hour False 20, hour False 4)
              "रात्र"     -> (hour False 20, hour False 4)
              _          -> (hour False 12, hour False 19)
        td <- interval TTime.Open start end
        tt $ partOfDay $ mkLatent td
      _ -> Nothing
  }

rulePODTOD :: Rule
rulePODTOD = Rule
  { name = "<part-of-day> <time-of-day>"
  , pattern =
    [ Predicate isAPartOfDay
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData{TTime.timePred = TTime.TimeIntervalsPredicate
        _ TTime.TimeDatePredicate{TTime.tdHour = Just (False, start)} _}:
       Token Time tod@TimeData{TTime.form = Just (TTime.TimeOfDay _ True)}:_) ->
        tt $ timeOfDayAMPM (start < 12) tod
      (Token Time TimeData{TTime.timePred = TTime.IntersectPredicate
        (TTime.TimeIntervalsPredicate
          _ TTime.TimeDatePredicate{TTime.tdHour = Just (False, start)} _) _}:
       Token Time tod@TimeData{TTime.form = Just (TTime.TimeOfDay _ True)}:_) ->
        tt $ timeOfDayAMPM (start < 12) tod
      (Token Time td1:Token Time td2:_) ->
        Token Time . notLatent <$> intersect td1 td2
      _ -> Nothing
  }

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

ruleIntersect :: Rule
ruleIntersect = Rule
  { name = "intersect"
  , pattern =
    [ Predicate $ isGrainFinerThan TG.Year
    , Predicate $ or . sequence [isNotLatent, isGrainOfTime TG.Year]
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:Token Time td2:_)
        | isLatentTOD td1 || isLatentTOD td2 -> Nothing
        | isCycle td1 || isCycle td2 -> Nothing
        | not (TTime.latent td1) || not (TTime.latent td2) ->
        Token Time . notLatent <$> intersect td1 td2
      _ -> Nothing
  }
  where
    isLatentTOD td = TTime.latent td && case TTime.form td of
      Just (TTime.TimeOfDay _ _) -> True
      _ -> False
    isCycle td = TTime.latent td && case TTime.form td of
      Nothing -> True
      _ -> False

ruleOrdinalDOW :: Rule
ruleOrdinalDOW = Rule
  { name = "<ordinal> <day-of-week>"
  , pattern =
    [ dimension Ordinal
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal od:Token Time td1:_) -> do
        let v = TOrdinal.value od
        let n = if v < 0 then v else v - 1
        tt $ predNth n False td1
      _ -> Nothing
  }

ruleNthTimeOfTime :: Rule
ruleNthTimeOfTime = Rule
  { name = "<time> च्या <ordinal> <day-of-week>"
  , pattern =
    [ Predicate $ not . isGrainFinerThan TG.Month
    , regex "च्या|चा|ची|चे|तील|ील|ला"
    , dimension Ordinal
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (Token Time td2:_:Token Ordinal od:Token Time td1:_) -> do
        let v = TOrdinal.value od
        let n = if v < 0 then v else v - 1
        Token Time . predNth n False <$> intersect td2 td1
      _ -> Nothing
  }

ruleNthTimeOfTimeNoPost :: Rule
ruleNthTimeOfTimeNoPost = Rule
  { name = "<time> <ordinal> <day-of-week>"
  , pattern =
    [ Predicate $ not . isGrainFinerThan TG.Month
    , dimension Ordinal
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (Token Time td2:Token Ordinal od:Token Time td1:_) -> do
        let v = TOrdinal.value od
        let n = if v < 0 then v else v - 1
        Token Time . predNth n False <$> intersect td2 td1
      _ -> Nothing
  }

ruleDurationBeforeTime :: Rule
ruleDurationBeforeTime = Rule
  { name = "<time> पासून <duration> पूर्वी"
  , pattern =
    [ Predicate isNotLatent
    , regex "(पासून|वरून|च्या|चा|ची|चे|तून)"
    , dimension Duration
    , regex "पूर्वी|आधी"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_:Token Duration dd:_:_) ->
        tt $ durationBefore dd td
      _ -> Nothing
  }

ruleDurationBeforeTimeNoPost :: Rule
ruleDurationBeforeTimeNoPost = Rule
  { name = "<time> <duration> पूर्वी|आधी"
  , pattern =
    [ Predicate isNotLatent
    , dimension Duration
    , regex "पूर्वी|आधी"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:Token Duration dd:_:_) ->
        tt $ durationBefore dd td
      _ -> Nothing
  }

ruleDurationBeforeTimeDirect :: Rule
ruleDurationBeforeTimeDirect = Rule
  { name = "<duration> पूर्वी <time>"
  , pattern =
    [ dimension Duration
    , regex "पूर्वी|आधी"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_:Token Time td:_) ->
        tt $ durationBefore dd td
      _ -> Nothing
  }

ruleIntervalTimeDurationKi :: Rule
ruleIntervalTimeDurationKi = Rule
  { name = "<time> <duration>"
  , pattern =
    [ Predicate isNotLatent
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:Token Duration dd:_) ->
        Token Time <$> interval TTime.Closed td1 (durationAfter dd td1)
      _ -> Nothing
  }

ruleIntervalTimeFillerDuration :: Rule
ruleIntervalTimeFillerDuration = Rule
  { name = "<time> <filler> <duration>"
  , pattern =
    [ Predicate isNotLatent
    , regex "[^0-9\\x{0966}-\\x{096f}\\n]{1,30}"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Duration dd:_) ->
        Token Time <$> interval TTime.Closed td1 (durationAfter dd td1)
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

ruleInDuration :: Rule
ruleInDuration = Rule
  { name = "<duration> मध्ये|त|ात"
  , pattern =
    [ dimension Duration
    , regex "मध्ये|ात|ंत|त|च्या आत"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_) -> tt $ inDuration dd
      _ -> Nothing
  }

-- Handles fused locative grains like "6 दिवसांत", "6 तासांत", "3 महिन्यांत"
-- where the locative ांत/ांना is already baked into the TimeGrain token.
ruleInDurationLocative :: Rule
ruleInDurationLocative = Rule
  { name = "<integer> <locative-grain>"
  , pattern =
    [ regex "(\\d+|[०-९]+)"
    , regex "(सेकंदांना|सेकंदांत|मिनिटांना|मिनिटांत|तासांना|तासांत|दिवसांना|दिवसांत|आठवड्यांत|महिन्यांत|वर्षांत)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m:_)):Token RegexMatch (GroupMatch (g:_)):_) ->
        let val = parseInt m
            grainVal = case g of
              "सेकंदांना" -> Just TG.Second
              "सेकंदांत"  -> Just TG.Second
              "मिनिटांना" -> Just TG.Minute
              "मिनिटांत"  -> Just TG.Minute
              "तासांना"   -> Just TG.Hour
              "तासांत"    -> Just TG.Hour
              "दिवसांना"  -> Just TG.Day
              "दिवसांत"   -> Just TG.Day
              "आठवड्यांत" -> Just TG.Week
              "महिन्यांत" -> Just TG.Month
              "वर्षांत"   -> Just TG.Year
              _            -> Nothing
        in case (val, grainVal) of
             (Just n, Just gr) -> tt $ inDuration $ DurationData n gr
             _ -> Nothing
      _ -> Nothing
  }



ruleDurationAfter :: Rule
ruleDurationAfter = Rule
  { name = "<duration> नंतर"
  , pattern =
    [ dimension Duration
    , regex "नंतर|पुढे"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_) -> tt $ withDirection TTime.After $ inDuration dd
      _ -> Nothing
  }

ruleDurationLocativeSuffixTime :: Rule
ruleDurationLocativeSuffixTime = Rule
  { name = "<integer> <locative-grain-suffix>"
  , pattern =
    [ Predicate isNatural
    , regex "(सेकंदांनी|सेकंदाने|मिनिटांनी|तासांनी|तासाने|दिवसांनी|आठवड्यांनी|महिन्यांनी|वर्षांनी)"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral TNumeral.NumeralData{TNumeral.value = v}:
       Token RegexMatch (GroupMatch (g:_)):_) -> do
         grainVal <- case g of
           "सेकंदांनी"  -> Just TG.Second
           "सेकंदाने"  -> Just TG.Second
           "मिनिटांनी" -> Just TG.Minute
           "तासांनी"   -> Just TG.Hour
           "तासाने"    -> Just TG.Hour
           "दिवसांनी"  -> Just TG.Day
           "आठवड्यांनी" -> Just TG.Week
           "महिन्यांनी" -> Just TG.Month
           "वर्षांनी"   -> Just TG.Year
           _           -> Nothing
         Just $ Token Time $ inDuration $ duration grainVal $ floor v
      _ -> Nothing
  }

ruleCompositeDurationWithLocativeSuffixTime :: Rule
ruleCompositeDurationWithLocativeSuffixTime = Rule
  { name = "<duration> आणि <integer> <locative-grain-suffix>"
  , pattern =
    [ dimension Duration
    , regex "आणि|,"
    , Predicate isNatural
    , regex "(सेकंदांनी|सेकंदाने|मिनिटांनी|तासांनी|तासाने|दिवसांनी|आठवड्यांनी|महिन्यांनी|वर्षांनी)"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:
       _:
       Token Numeral TNumeral.NumeralData{TNumeral.value = v}:
       Token RegexMatch (GroupMatch (g:_)):_) -> do
         grainVal <- case g of
           "सेकंदांनी"  -> Just TG.Second
           "सेकंदाने"  -> Just TG.Second
           "मिनिटांनी" -> Just TG.Minute
           "तासांनी"   -> Just TG.Hour
           "तासाने"    -> Just TG.Hour
           "दिवसांनी"  -> Just TG.Day
           "आठवड्यांनी" -> Just TG.Week
           "महिन्यांनी" -> Just TG.Month
           "वर्षांनी"   -> Just TG.Year
           _           -> Nothing
         let lastDuration = duration grainVal $ floor v
         Just $ Token Time $ inDuration $ dd <> lastDuration
      _ -> Nothing
  }

ruleDurationAgo :: Rule
ruleDurationAgo = Rule
  { name = "<duration> पूर्वी"
  , pattern =
    [ dimension Duration
    , regex "पूर्वी|आधी"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_) -> tt $ durationAgo dd
      _ -> Nothing
  }

adjustTOD12H :: TimeData -> TimeData -> Maybe TimeData
adjustTOD12H td1 td2@TimeData{TTime.form = Just (TTime.TimeOfDay (Just h2) True)}
  | isPM td1 && h2 < 12 = Just $ timeOfDayAMPM False td2
  | isAM td1 && h2 < 12 = Just $ timeOfDayAMPM True td2
  where
    isPM TimeData{TTime.form = Just (TTime.TimeOfDay (Just h1) _)} = h1 >= 12
    isPM _ = False
    isAM TimeData{TTime.form = Just (TTime.TimeOfDay (Just h1) _)} = h1 < 12
    isAM _ = False
adjustTOD12H _ td2 = Just td2

ruleIntervalTeDarmiyan :: Rule
ruleIntervalTeDarmiyan = Rule
  { name = "<time> ते <time> दरम्यान"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "ते|पासून"
    , Predicate isATimeOfDay
    , regex "दरम्यान|दरम्याने"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) -> do
        td2' <- adjustTOD12H td1 td2
        Token Time <$> interval TTime.Closed td1 td2'
      _ -> Nothing
  }

ruleIntervalTeParyant :: Rule
ruleIntervalTeParyant = Rule
  { name = "<time> ते <time> पर्यंत"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "ते"
    , Predicate isATimeOfDay
    , regex "पर्यंत"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) -> do
        td2' <- adjustTOD12H td1 td2
        Token Time <$> interval TTime.Closed td1 td2'
      _ -> Nothing
  }

ruleIntervalTe :: Rule
ruleIntervalTe = Rule
  { name = "<time> ते <time>"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "ते"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) -> do
        td2' <- adjustTOD12H td1 td2
        Token Time <$> interval TTime.Closed td1 td2'
      _ -> Nothing
  }

ruleIntervalDarmiyan :: Rule
ruleIntervalDarmiyan = Rule
  { name = "दरम्यान <time> आणि/ते <time>"
  , pattern =
    [ regex "दरम्यान|दरम्याने"
    , Predicate isATimeOfDay
    , regex "आणि|ते"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) -> do
        td2' <- adjustTOD12H td1 td2
        Token Time <$> interval TTime.Closed td1 td2'
      _ -> Nothing
  }

ruleDOMMonth :: Rule
ruleDOMMonth = Rule
  { name = "<day-of-month> <named-month>"
  , pattern =
    [ Predicate isDOMValue
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (token:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleDOMMonthYear :: Rule
ruleDOMMonthYear = Rule
  { name = "<day-of-month> <named-month> ,? <year>"
  , pattern =
    [ Predicate isDOMValue
    , Predicate isAMonth
    , regex ",|\\s+"
    , regex "(\\d{2,4}|[०-९]{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (token:
       Token Time td:
       _:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> do
         intVal <- parseInt $ Text.map devanagariToArabChar match
         dom <- intersectDOM td token
         Token Time <$> intersect dom (year intVal)
      _ -> Nothing
  }

ruleDateRozi :: Rule
ruleDateRozi = Rule
  { name = "<date> रोजी|ला"
  , pattern =
    [ Predicate $ isGrainOfTime TG.Day
    , regex "रोजी|ला"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }
rulePrecisionTOD :: Rule
rulePrecisionTOD = Rule
  { name = "about|exactly <time-of-day>"
  , pattern =
    [ regex "(सुमारे|साधारण|अंदाजे|जवळपास|नेमके|फक्त)"
    , Predicate $ isGrainFinerThan TG.Year
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }
ruleAMPM :: Rule
ruleAMPM = Rule
  { name = "<time-of-day> am|pm"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "(?i)(a|p)\\.?m?\\.?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:Token RegexMatch (GroupMatch (ap:_)):_) ->
        tt $ timeOfDayAMPM (Text.toLower ap == "a") td
      _ -> Nothing
  }

timezoneName :: String
timezoneName = "YEKT|YEKST|YAKT|YAKST|WITA|WIT|WIB|WGT|WGST|WFT|WET|WEST|WAT|WAST|VUT|VLAT|VLAST|VET|UZT|UYT|UYST|UTC|ULAT|TVT|TMT|TLT|TKT|TJT|TFT|TAHT|SST|SRT|SGT|SCT|SBT|SAST|SAMT|RET|PYT|PYST|PWT|PST|PONT|PMST|PMDT|PKT|PHT|PHOT|PGT|PETT|PETST|PET|PDT|OMST|OMSST|NZST|NZDT|NUT|NST|NPT|NOVT|NOVST|NFT|NDT|NCT|MYT|MVT|MUT|MST|MSK|MSD|MMT|MHT|MDT|MAWT|MART|MAGT|MAGST|LINT|LHST|LHDT|KUYT|KST|KRAT|KRAST|KGT|JST|IST|IRST|IRKT|IRKST|IRDT|IOT|IDT|ICT|HOVT|HKT|GYT|GST|GMT|GILT|GFT|GET|GAMT|GALT|FNT|FKT|FKST|FJT|FJST|EST|EGT|EGST|EET|EEST|EDT|ECT|EAT|EAST|EASST|DAVT|ChST|CXT|CVT|CST|COT|CLT|CLST|CKT|CHAST|CHADT|CET|CEST|CDT|CCT|CAT|CAST|BTT|BST|BRT|BRST|BOT|BNT|AZT|AZST|AZOT|AZOST|AWST|AWDT|AST|ART|AQTT|ANAT|ANAST|AMT|AMST|ALMT|AKST|AKDT|AFT|AEST|AEDT|ADT|ACST|ACDT"

ruleTimezone :: Rule
ruleTimezone = Rule
  { name = "<time> timezone"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isATimeOfDay, hasNoTimezone]
    , regex $ "\\b(" ++ timezoneName ++ ")\\b"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:
       Token RegexMatch (GroupMatch (tz:_)):
       _) -> Token Time <$> inTimezone (Text.toUpper tz) td
      _ -> Nothing
  }

ruleThisTimeGrain :: Rule
ruleThisTimeGrain = Rule
  { name = "this <time-grain>"
  , pattern =
    [ regex "या|ह्या"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) ->
        tt $ cycleNth grain 0
      _ -> Nothing
  }

ruleNextTimeGrain :: Rule
ruleNextTimeGrain = Rule
  { name = "next <time-grain>"
  , pattern =
    [ regex "येत्या|पुढील|पुढच्या|पुढचा|पुढची|येणारा|आगामी"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) ->
        tt $ cycleNth grain 1
      _ -> Nothing
  }

ruleLastTimeGrain :: Rule
ruleLastTimeGrain = Rule
  { name = "last <time-grain>"
  , pattern =
    [ regex "गेल्या|मागील|मागच्या|मागचा|मागची"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) ->
        tt . cycleNth grain $ -1
      _ -> Nothing
  }

ruleSavaHOD :: Rule
ruleSavaHOD = Rule
  { name = "सव्वा <hour-of-day>"
  , pattern =
    [ regex "सव्वा"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time TimeData{TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:_) ->
        tt $ hourMinute is12H hours 15
      _ -> Nothing
  }

ruleSaadeHOD :: Rule
ruleSaadeHOD = Rule
  { name = "साडे <hour-of-day>"
  , pattern =
    [ regex "साडे"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time TimeData{TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:_) ->
        tt $ hourMinute is12H hours 30
      _ -> Nothing
  }

parseHourWord :: Text.Text -> Maybe Int
parseHourWord word = case word of
  "एक"   -> Just 1
  "दोन"  -> Just 2
  "तीन"  -> Just 3
  "चार"  -> Just 4
  "पाच"  -> Just 5
  "सहा"  -> Just 6
  "सात"  -> Just 7
  "आठ"  -> Just 8
  "नऊ"  -> Just 9
  "दहा"  -> Just 10
  "अकरा" -> Just 11
  "बारा" -> Just 12
  _      -> Nothing

ruleSavaHODMerged :: Rule
ruleSavaHODMerged = Rule
  { name = "सव्वा<hour-of-day> (merged)"
  , pattern =
    [ regex "सव्वा(एक|दोन|तीन|चार|पाच|सहा|सात|आठ|नऊ|दहा|अकरा|बारा)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (h:_)):_) -> do
        val <- parseHourWord h
        tt $ hourMinute True val 15
      _ -> Nothing
  }

ruleSaadeHODMerged :: Rule
ruleSaadeHODMerged = Rule
  { name = "साडे<hour-of-day> (merged)"
  , pattern =
    [ regex "साडे(तीन|चार|पाच|सहा|सात|आठ|नऊ|दहा|अकरा|बारा)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (h:_)):_) -> do
        val <- parseHourWord h
        tt $ hourMinute True val 30
      _ -> Nothing
  }

rulePauneHODMerged :: Rule
rulePauneHODMerged = Rule
  { name = "पावणे<hour-of-day> (merged)"
  , pattern =
    [ regex "पावणे(एक|दोन|तीन|चार|पाच|सहा|सात|आठ|नऊ|दहा|अकरा|बारा)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (h:_)):_) -> do
        val <- parseHourWord h
        let targetHour = if val == 1 then 12 else val - 1
        tt $ hourMinute True targetHour 45
      _ -> Nothing
  }

ruleDedhHOD :: Rule
ruleDedhHOD = Rule
  { name = "दीड वाजता (1:30)"
  , pattern =
    [ regex "दीड(\\s*(?:वाजता|वाजेता))?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        if Text.isInfixOf "वाज" match
          then tt $ hourMinute True 1 30
          else Nothing
      _ -> Nothing
  }

ruleDhaiHOD :: Rule
ruleDhaiHOD = Rule
  { name = "अडीच वाजता (2:30)"
  , pattern =
    [ regex "अडीच(\\s*(?:वाजता|वाजेता))?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        if Text.isInfixOf "वाज" match
          then tt $ hourMinute True 2 30
          else Nothing
      _ -> Nothing
  }

rulePauneHOD :: Rule
rulePauneHOD = Rule
  { name = "पावणे <hour-of-day>"
  , pattern =
    [ regex "पावणे"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> minutesBefore 15 td
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleToday
  , ruleTomorrow
  , ruleDayAfterTomorrow
  , ruleEveryCycle
  , ruleNextMonth
  , ruleNextDOW
  , ruleLastDOW
  , ruleThisDOW
  , ruleNoonMidnight
  , ruleHHMM
  , ruleTODLatent
  , ruleVajta
  , ruleSampurnaTime
  , rulePartOfDays
  , rulePODTOD
  , ruleTimePOD
  , ruleIntersect
  , ruleOrdinalDOW
  , ruleNthTimeOfTime
  , ruleNthTimeOfTimeNoPost
  , ruleDurationBeforeTime
  , ruleDurationBeforeTimeNoPost
  , ruleDurationBeforeTimeDirect
  , ruleIntervalTimeDurationKi
  , ruleIntervalTimeFillerDuration
  , ruleInDuration
  , ruleInDurationLocative
  , ruleDurationAfter
  , ruleDurationLocativeSuffixTime
  , ruleCompositeDurationWithLocativeSuffixTime
  , ruleDurationAgo
  , ruleIntervalTeDarmiyan
  , ruleIntervalTeParyant
  , ruleIntervalTe
  , ruleIntervalDarmiyan
  , ruleDOMMonth
  , ruleDOMMonthYear
  , ruleDateRozi
  , ruleAMPM
  , rulePrecisionTOD
  , ruleTimezone
  , ruleThisTimeGrain
  , ruleNextTimeGrain
  , ruleLastTimeGrain
  , ruleSavaHOD
  , ruleSavaHODMerged
  , ruleSaadeHOD
  , ruleSaadeHODMerged
  , ruleDedhHOD
  , ruleDhaiHOD
  , rulePauneHOD
  , rulePauneHODMerged
  ]
  ++ ruleDaysOfWeek
  ++ ruleMonths
