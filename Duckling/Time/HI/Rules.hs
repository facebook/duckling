{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.HI.Rules where

import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
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
    [ regex "आज"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 0
  }

ruleTomorrow :: Rule
ruleTomorrow = Rule
  { name = "tomorrow"
  , pattern =
    [ regex "कल|आने वाला कल"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 1
  }

ruleDayAfterTomorrow :: Rule
ruleDayAfterTomorrow = Rule
  { name = "day after tomorrow"
  , pattern =
    [ regex "परसों"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 2
  }

ruleEveryCycle :: Rule
ruleEveryCycle = Rule
  { name = "every <cycle>"
  , pattern =
    [ regex "हर|प्रत्येक"
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
    [ regex "अगले|अगला|अगली|आने वाले|आने वाला|आने वाली|आगामी"
    , regex "महीने|महीना|माह"
    ]
  , prod = \_ -> tt $ cycleNth TG.Month 1
  }

ruleNextDOW :: Rule
ruleNextDOW = Rule
  { name = "next <day-of-week>"
  , pattern =
    [ regex "अगले|अगला|अगली|आने वाले|आने वाला|आने वाली|आगामी"
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
    [ regex "पिछले|पिछला|पिछली"
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
    [ regex "इस|इसकी"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time dow:_) -> tt $ predNth 0 False dow
      _ -> Nothing
  }

ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = mkRuleDaysOfWeek
  [ ( "सोमवार", "सोमवार|सोम\\.?" )
  , ( "मंगलवार", "मंगलवार|मंगल\\.?" )
  , ( "बुधवार", "बुधवार|बुध\\.?" )
  , ( "गुरुवार", "गुरुवार|गुरु|बृहस्पतिवार" )
  , ( "शुक्रवार", "शुक्रवार|शुक्र\\.?" )
  , ( "शनिवार", "शनिवार|शनि\\.?" )
  , ( "रविवार", "रविवार|रवि|इतवार" )
  ]

ruleMonths :: [Rule]
ruleMonths = mkRuleMonths
  [ ( "जनवरी", "जनवरी( को| में)?|जन\\.?" )
  , ( "फ़रवरी", "फ़रवरी( को| में)?|फरवरी( को| में)?|फ़र\\.?" )
  , ( "मार्च", "मार्च( को| में)?" )
  , ( "अप्रैल", "अप्रैल( को| में)?|अप्रै\\.?" )
  , ( "मई", "मई( को| में)?" )
  , ( "जून", "जून( को| में)?" )
  , ( "जुलाई", "जुलाई( को| में)?" )
  , ( "अगस्त", "अगस्त( को| में)?|अग\\.?" )
  , ( "सितंबर", "सितंबर( को| में)?|सितम्बर( को| में)?|सित\\.?" )
  , ( "अक्टूबर", "अक्टूबर( को| में)?|अक्टू\\.?" )
  , ( "नवंबर", "नवंबर( को| में)?|नवम्बर( को| में)?|नव\\.?" )
  , ( "दिसंबर", "दिसंबर( को| में)?|दिसम्बर( को| में)?|दिस\\.?" )
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

ruleBaje :: Rule
ruleBaje = Rule
  { name = "<time-of-day> बजे"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "बजे"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

rulePartOfDays :: Rule
rulePartOfDays = Rule
  { name = "part of days"
  , pattern =
    [ regex "(सुबह|सवेरे|प्रातः|दोपहर|शाम|संध्या|रात)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        let (start, end) = case match of
              "सुबह"   -> (hour False 4, hour False 12)
              "सवेरे"  -> (hour False 4, hour False 12)
              "प्रातः" -> (hour False 4, hour False 12)
              "दोपहर" -> (hour False 12, hour False 16)
              "शाम"   -> (hour False 16, hour False 20)
              "संध्या" -> (hour False 16, hour False 20)
              "रात"    -> (hour False 20, hour False 4)
              _       -> (hour False 12, hour False 19)
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
  { name = "<time> के <ordinal> <day-of-week>"
  , pattern =
    [ Predicate $ not . isGrainFinerThan TG.Month
    , regex "के|का|की|में"
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
  { name = "<time> से <duration> पहले"
  , pattern =
    [ Predicate isNotLatent
    , regex "(से|के|की|का)"
    , dimension Duration
    , regex "पहले|पूर्व"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_:Token Duration dd:_:_) ->
        tt $ durationBefore dd td
      _ -> Nothing
  }

ruleDurationBeforeTimeNoPost :: Rule
ruleDurationBeforeTimeNoPost = Rule
  { name = "<time> <duration> पहले"
  , pattern =
    [ Predicate isNotLatent
    , dimension Duration
    , regex "पहले|पूर्व"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:Token Duration dd:_:_) ->
        tt $ durationBefore dd td
      _ -> Nothing
  }

ruleDurationBeforeTimeDirect :: Rule
ruleDurationBeforeTimeDirect = Rule
  { name = "<duration> पहले <time>"
  , pattern =
    [ dimension Duration
    , regex "पहले|पूर्व"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_:Token Time td:_) ->
        tt $ durationBefore dd td
      _ -> Nothing
  }

ruleIntervalTimeDurationKi :: Rule
ruleIntervalTimeDurationKi = Rule
  { name = "<time> <duration> की"
  , pattern =
    [ Predicate isNotLatent
    , dimension Duration
    , regex "की|के लिए|का|के|तक"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:Token Duration dd:_:_) ->
        Token Time <$> interval TTime.Closed td1 (durationAfter dd td1)
      _ -> Nothing
  }

ruleIntervalTimeFillerDuration :: Rule
ruleIntervalTimeFillerDuration = Rule
  { name = "<time> ... <duration>"
  , pattern =
    [ Predicate isNotLatent
    , regex "[^\\d]+"
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
  { name = "<duration> में|के अंदर"
  , pattern =
    [ dimension Duration
    , regex "में|के अंदर|के भीतर"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_) -> tt $ inDuration dd
      _ -> Nothing
  }

ruleDurationAfter :: Rule
ruleDurationAfter = Rule
  { name = "<duration> के बाद"
  , pattern =
    [ dimension Duration
    , regex "के बाद|बाद|के पश्चात|पश्चात"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_) -> tt $ withDirection TTime.After $ inDuration dd
      _ -> Nothing
  }

ruleDurationAgo :: Rule
ruleDurationAgo = Rule
  { name = "<duration> पहले"
  , pattern =
    [ dimension Duration
    , regex "पहले|पूर्व"
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

ruleIntervalSeBeech :: Rule
ruleIntervalSeBeech = Rule
  { name = "<time> से <time> के बीच"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "से(?: लेकर)?"
    , Predicate isATimeOfDay
    , regex "के (?:बीच|मध्य)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) -> do
        td2' <- adjustTOD12H td1 td2
        Token Time <$> interval TTime.Closed td1 td2'
      _ -> Nothing
  }

ruleIntervalSeTak :: Rule
ruleIntervalSeTak = Rule
  { name = "<time> से <time> तक"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "से(?: लेकर)?"
    , Predicate isATimeOfDay
    , regex "तक"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) -> do
        td2' <- adjustTOD12H td1 td2
        Token Time <$> interval TTime.Closed td1 td2'
      _ -> Nothing
  }

ruleIntervalSe :: Rule
ruleIntervalSe = Rule
  { name = "<time> से <time>"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "से"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) -> do
        td2' <- adjustTOD12H td1 td2
        Token Time <$> interval TTime.Closed td1 td2'
      _ -> Nothing
  }

ruleIntervalKeBeech :: Rule
ruleIntervalKeBeech = Rule
  { name = "के बीच <time> और/से <time>"
  , pattern =
    [ regex "(?:के )?(?:बीच|मध्य)"
    , Predicate isATimeOfDay
    , regex "और|से"
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

ruleDateKo :: Rule
ruleDateKo = Rule
  { name = "<date> को"
  , pattern =
    [ Predicate $ isGrainOfTime TG.Day
    , regex "को"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ td { TTime.latent = False }
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
    [ regex "इस|इन्हीं"
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
    [ regex "अगले|अगला|अगली|आने वाले|आने वाला|आने वाली|आगामी"
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
    [ regex "पिछले|पिछला|पिछली"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) ->
        tt . cycleNth grain $ -1
      _ -> Nothing
  }

ruleSavaHOD :: Rule
ruleSavaHOD = Rule
  { name = "सवा <hour-of-day>"
  , pattern =
    [ regex "सवा"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time TimeData{TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:_) ->
        tt $ hourMinute is12H hours 15
      _ -> Nothing
  }

ruleSaadeHOD :: Rule
ruleSaadeHOD = Rule
  { name = "साढ़े <hour-of-day>"
  , pattern =
    [ regex "साढ़े|साड़े"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time TimeData{TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:_) ->
        tt $ hourMinute is12H hours 30
      _ -> Nothing
  }

ruleDedhHOD :: Rule
ruleDedhHOD = Rule
  { name = "डेढ़ बजे (1:30)"
  , pattern =
    [ regex "डेढ़(\\s*(?:बजे|बजेता))?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        if Text.isInfixOf "बजे" match
          then tt $ hourMinute True 1 30
          else Nothing
      _ -> Nothing
  }

ruleDhaiHOD :: Rule
ruleDhaiHOD = Rule
  { name = "ढाई बजे (2:30)"
  , pattern =
    [ regex "ढाई(\\s*(?:बजे|बजेता))?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        if Text.isInfixOf "बजे" match
          then tt $ hourMinute True 2 30
          else Nothing
      _ -> Nothing
  }

rulePauneHOD :: Rule
rulePauneHOD = Rule
  { name = "पौने <hour-of-day>"
  , pattern =
    [ regex "पौने"
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
  , ruleHHMM
  , ruleTODLatent
  , ruleBaje
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
  , ruleDurationAfter
  , ruleDurationAgo
  , ruleIntervalSeBeech
  , ruleIntervalSeTak
  , ruleIntervalSe
  , ruleIntervalKeBeech
  , ruleDOMMonth
  , ruleDOMMonthYear
  , ruleDateKo
  , ruleAMPM
  , ruleTimezone
  , ruleThisTimeGrain
  , ruleNextTimeGrain
  , ruleLastTimeGrain
  , ruleSavaHOD
  , ruleSaadeHOD
  , ruleDedhHOD
  , ruleDhaiHOD
  , rulePauneHOD
  ]
  ++ ruleDaysOfWeek
  ++ ruleMonths


