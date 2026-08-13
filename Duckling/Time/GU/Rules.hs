{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.GU.Rules where

import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Regex.Types
import Duckling.Duration.Helpers (duration)
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
    [ regex "આજે|આજ"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 0
  }

ruleTomorrow :: Rule
ruleTomorrow = Rule
  { name = "tomorrow"
  , pattern =
    [ regex "આવતીકાલે|આવતીકાલ|કાલે|કાલ"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 1
  }

ruleDayAfterTomorrow :: Rule
ruleDayAfterTomorrow = Rule
  { name = "day after tomorrow"
  , pattern =
    [ regex "પરમદિવસે|પરમદિવસ"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 2
  }

ruleEveryCycle :: Rule
ruleEveryCycle = Rule
  { name = "every <cycle>"
  , pattern =
    [ regex "દર|દરેક|પ્રત્યેક"
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
    [ regex "આવતા|આવતો|આવતી|આવતું"
    , regex "મહિના|મહિનામાં|મહિનો|માસ"
    ]
  , prod = \_ -> tt $ cycleNth TG.Month 1
  }

ruleNextDOW :: Rule
ruleNextDOW = Rule
  { name = "next <day-of-week>"
  , pattern =
    [ regex "આવતા|આવતી|આવતો|આગામી|પછીના|આવનાર"
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
    [ regex "ગયા|ગયી|ગયો|પાછલા|ગયેલ"
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
    [ regex "આ|આજ"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time dow:_) -> tt $ predNth 0 False dow
      _ -> Nothing
  }

ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = mkRuleDaysOfWeek
  [ ( "સોમવાર", "સોમવારે|સોમવારના|સોમવારની|સોમવારનો|સોમવારનું|સોમવારથી|સોમવાર|સોમ\\.?" )
  , ( "મંગળવાર", "મંગળવારે|મંગળવારના|મંગળવારની|મંગળવારનો|મંગળવારનું|મંગળવારથી|મંગળવાર|મંગળ\\.?" )
  , ( "બુધવાર", "બુધવારે|બુધવારના|બુધવારની|બુધવારનો|બુધવારનું|બુધવારથી|બુધવાર|બુધ\\.?" )
  , ( "ગુરુવાર", "ગુરુવારે|ગુરુવારના|ગુરુવારની|ગુરુવારનો|ગુરુવારનું|ગુરુવારથી|ગુરુવાર|ગુરુ" )
  , ( "शુક્રવાર", "શુક્રવારે|શુક્રવારના|શુક્રવારની|શુક્રવારનો|શુક્રવારનું|શુક્રવારથી|શુક્રવાર|શુક્ર\\.?" )
  , ( "શનિવાર", "શનિવારે|શનિવારના|શનિવારની|શનિવારનો|શનિવારનું|શનિવારથી|શનિવાર|શનિ\\.?" )
  , ( "રવિવાર", "રવિવારે|રવિવારના|રવિવારની|રવિવારનો|રવિવારનું|રવિવારથી|રવિવાર|રવિ" )
  ]

ruleMonths :: [Rule]
ruleMonths = mkRuleMonths
  [ ( "જાન્યુઆરી", "જાન્યુઆરી(માં)?|જાન્યુ\\.?" )
  , ( "ફેબ્રુઆરી", "ફેબ્રુઆરી(માં)?|ફેબ્રુ\\.?" )
  , ( "માર્ચ", "માર્ચ(માં)?" )
  , ( "એપ્રિલ", "એપ્રિલ(માં)?" )
  , ( "મે", "મે(માં)?" )
  , ( "જૂન", "જૂન(માં)?" )
  , ( "જુલાઈ", "જુલાઈ(માં)?" )
  , ( "ઓગસ્ટ", "ઓગસ્ટે|ઑગસ્ટે|ઓગસ્ટ(માં)?|ઑગસ્ટ(માં)?|ઓગષ્ટ(માં)?|ઑગષ્ટ(માં)?|ઓગષ્ટ|ઑગષ્ટ|ઓગસ્ટ|ઑગસ્ટ|ઓગ\\.?|ઑગ\\.?" )
  , ( "સપ્ટેમ્બર", "સપ્ટેમ્બરે|સપ્ટેમ્બર(માં)?|સપ્ટે\\.?" )
  , ( "ઓક્ટોબર", "ઓક્ટોબરે|ઓક્ટોબર(માં)?|ઓક્ટો\\.?" )
  , ( "નવેમ્બર", "નવેમ્બરે|નવેમ્બર(માં)?|નવે\\.?" )
  , ( "ડિસેમ્બર", "ડિસેમ્બરે|ડિસેમ્બર(માં)?|ડિસે\\.?" )
  ]

gujaratiToArabChar :: Char -> Char
gujaratiToArabChar c = case c of
  '૦' -> '0'
  '૧' -> '1'
  '૨' -> '2'
  '૩' -> '3'
  '૪' -> '4'
  '૫' -> '5'
  '૬' -> '6'
  '૭' -> '7'
  '૮' -> '8'
  '૯' -> '9'
  _   -> c

ruleHHMM :: Rule
ruleHHMM = Rule
  { name = "hh:mm"
  , pattern = [regex "((?:[01]?\\d)|(?:2[0-3])|(?:[૦૧]?[૦-૯])|(?:૨[૦-૩]))[:.]([0-5]\\d|[૦-૫][૦-૯])"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (hh:mm:_)):_) -> do
        h <- parseInt $ Text.map gujaratiToArabChar hh
        m <- parseInt $ Text.map gujaratiToArabChar mm
        tt $ hourMinute (h < 12) h m
      _ -> Nothing
  }

ruleVagye :: Rule
ruleVagye = Rule
  { name = "<time-of-day> વાગ્યે|વાગે"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "વાગ્યે|વાગે|કલાકે"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

rulePartOfDays :: Rule
rulePartOfDays = Rule
  { name = "part of days"
  , pattern =
    [ regex "(સવારે|સવાર|પ્રાતઃ|બપોરે|બપોર|સાંજે|સાંજ|સંધ્યા|રાત્રે|રાત)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        let (start, end) = case match of
              "સવારે"  -> (hour False 4, hour False 12)
              "સવાર"   -> (hour False 4, hour False 12)
              "પ્રાતઃ" -> (hour False 4, hour False 12)
              "બપોરે"  -> (hour False 12, hour False 16)
              "બપોર"   -> (hour False 12, hour False 16)
              "સાંજે"  -> (hour False 16, hour False 20)
              "સાંજ"   -> (hour False 16, hour False 20)
              "સંધ્યા" -> (hour False 16, hour False 20)
              "રાત્રે"  -> (hour False 20, hour False 4)
              "રાત"    -> (hour False 20, hour False 4)
              _        -> (hour False 12, hour False 19)
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

ruleNthTimeOfTime :: Rule
ruleNthTimeOfTime = Rule
  { name = "<time> ના <ordinal> <day-of-week>"
  , pattern =
    [ Predicate $ not . isGrainFinerThan TG.Month
    , regex "ના|નો|ની|નું|માં"
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

ruleDurationBeforeTime :: Rule
ruleDurationBeforeTime = Rule
  { name = "<time> થી <duration> પહેલાં"
  , pattern =
    [ Predicate isNotLatent
    , regex "(થી|માંથી|ના|નો|ની|નું)"
    , dimension Duration
    , regex "પહેલાં|પહેલા|અગાઉ"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_:Token Duration dd:_:_) ->
        tt $ durationBefore dd td
      _ -> Nothing
  }

ruleDurationBeforeTimeNoPost :: Rule
ruleDurationBeforeTimeNoPost = Rule
  { name = "<time> <duration> પહેલાં"
  , pattern =
    [ Predicate isNotLatent
    , dimension Duration
    , regex "પહેલાં|પહેલા|અગાઉ"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:Token Duration dd:_:_) ->
        tt $ durationBefore dd td
      _ -> Nothing
  }

ruleDurationBeforeTimeDirect :: Rule
ruleDurationBeforeTimeDirect = Rule
  { name = "<duration> પહેલાં <time>"
  , pattern =
    [ dimension Duration
    , regex "પહેલાં|પહેલા"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_:Token Time td:_) ->
        tt $ durationBefore dd td
      _ -> Nothing
  }

ruleIntervalTimeDurationKi :: Rule
ruleIntervalTimeDurationKi = Rule
  { name = "<time> <duration> ની"
  , pattern =
    [ Predicate isNotLatent
    , dimension Duration
    , regex "ની|ના|નો|નું|માટે|સુધી"
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
  { name = "<duration> માં|ની અંદર"
  , pattern =
    [ dimension Duration
    , regex "માં|ની અંદર|ની ભીતર"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_) -> tt $ inDuration dd
      _ -> Nothing
  }

ruleInDurationLocative :: Rule
ruleInDurationLocative = Rule
  { name = "<integer> <locative-grain>"
  , pattern =
    [ regex "(\\d+|[૦-૯]+)"
    , regex "(સેકન્ડમાં|સેકંડમાં|મિનિટમાં|કલાકમાં|દિવસમાં|અઠવાડિયામાં|મહિનામાં|વર્ષમાં)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m:_)):Token RegexMatch (GroupMatch (g:_)):_) -> do
        val <- parseInt m
        grainVal <- case g of
          "સેકન્ડમાં"  -> Just TG.Second
          "સેકંડમાં"   -> Just TG.Second
          "મિનિટમાં"   -> Just TG.Minute
          "કલાકમાં"    -> Just TG.Hour
          "દિવસમાં"    -> Just TG.Day
          "અઠવાડિયામાં" -> Just TG.Week
          "મહિનામાં"   -> Just TG.Month
          "વર્ષમાં"    -> Just TG.Year
          _ -> Nothing
        Just $ Token Time $ inDuration (duration grainVal val)
      _ -> Nothing
  }

ruleDurationAfter :: Rule
ruleDurationAfter = Rule
  { name = "<duration> પછી"
  , pattern =
    [ dimension Duration
    , regex "પછી|બાદ"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_) -> tt $ withDirection TTime.After $ inDuration dd
      _ -> Nothing
  }

ruleDurationAgo :: Rule
ruleDurationAgo = Rule
  { name = "<duration> પહેલાં"
  , pattern =
    [ dimension Duration
    , regex "પહેલાં|પહેલા"
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

ruleIntervalThiVachche :: Rule
ruleIntervalThiVachche = Rule
  { name = "<time> થી <time> વચ્ચે"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "થી"
    , Predicate isATimeOfDay
    , regex "વચ્ચે"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) -> do
        td2' <- adjustTOD12H td1 td2
        Token Time <$> interval TTime.Closed td1 td2'
      _ -> Nothing
  }

ruleIntervalThiSudhi :: Rule
ruleIntervalThiSudhi = Rule
  { name = "<time> થી <time> સુધી"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "થી"
    , Predicate isATimeOfDay
    , regex "સુધી"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) -> do
        td2' <- adjustTOD12H td1 td2
        Token Time <$> interval TTime.Closed td1 td2'
      _ -> Nothing
  }

ruleIntervalThi :: Rule
ruleIntervalThi = Rule
  { name = "<time> થી <time>"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "થી"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) -> do
        td2' <- adjustTOD12H td1 td2
        Token Time <$> interval TTime.Closed td1 td2'
      _ -> Nothing
  }

ruleIntervalVachche :: Rule
ruleIntervalVachche = Rule
  { name = "વચ્ચે <time> અને/થી <time>"
  , pattern =
    [ regex "વચ્ચે"
    , Predicate isATimeOfDay
    , regex "અને|થી"
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
    , regex "(\\d{2,4}|[૦-૯]{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (token:
       Token Time td:
       _:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> do
         intVal <- parseInt $ Text.map gujaratiToArabChar match
         dom <- intersectDOM td token
         Token Time <$> intersect dom (year intVal)
      _ -> Nothing
  }

ruleDateE :: Rule
ruleDateE = Rule
  { name = "<date> એ|ના રોજ"
  , pattern =
    [ Predicate $ isGrainOfTime TG.Day
    , regex "એ|ના રોજ"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
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
    [ regex "આ|આજ"
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
    [ regex "આવતા|આવતી|આવતો|આગામી|પછીના"
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
    [ regex "ગયા|ગયી|ગયો|પાછલા"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) ->
        tt . cycleNth grain $ -1
      _ -> Nothing
  }

ruleSavaHOD :: Rule
ruleSavaHOD = Rule
  { name = "સવા <hour-of-day>"
  , pattern =
    [ regex "સવા"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time TimeData{TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:_) ->
        tt $ hourMinute is12H hours 15
      _ -> Nothing
  }

ruleSaadeHOD :: Rule
ruleSaadeHOD = Rule
  { name = "સાડા <hour-of-day>"
  , pattern =
    [ regex "સાડા|સાડાં"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time TimeData{TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:_) ->
        tt $ hourMinute is12H hours 30
      _ -> Nothing
  }

ruleDedhHOD :: Rule
ruleDedhHOD = Rule
  { name = "દોઢ વાગ્યે (1:30)"
  , pattern =
    [ regex "દોઢ(\\s*(?:વાગ્યે|વાગ્યા))?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        if Text.isInfixOf "વાગ" match
          then tt $ hourMinute True 1 30
          else Nothing
      _ -> Nothing
  }

ruleDhaiHOD :: Rule
ruleDhaiHOD = Rule
  { name = "અઢી વાગ્યે (2:30)"
  , pattern =
    [ regex "અઢી(\\s*(?:વાગ્યે|વાગ્યા))?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        if Text.isInfixOf "વાગ" match
          then tt $ hourMinute True 2 30
          else Nothing
      _ -> Nothing
  }

rulePauneHOD :: Rule
rulePauneHOD = Rule
  { name = "પોણા <hour-of-day>"
  , pattern =
    [ regex "પોણા|પોણો"
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
  , ruleVagye
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
  , ruleDurationAgo
  , ruleIntervalThiVachche
  , ruleIntervalThiSudhi
  , ruleIntervalThi
  , ruleIntervalVachche
  , ruleDOMMonth
  , ruleDOMMonthYear
  , ruleDateE
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
