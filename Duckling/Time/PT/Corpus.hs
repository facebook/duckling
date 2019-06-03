-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.PT.Corpus
  ( corpus
  , negativeCorpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types hiding (examples)
import Duckling.Time.Corpus
import Duckling.Time.Types hiding (Month)
import Duckling.TimeGrain.Types hiding (add)

corpus :: Corpus
corpus = (testContext {locale = makeLocale PT Nothing}, testOptions, allExamples)

negativeCorpus :: NegativeCorpus
negativeCorpus = (testContext {locale = makeLocale PT Nothing}, testOptions, examples)
  where
    examples =
      [ "no 987"
      , "um"
      , "um dos"
      , "um dos minutos"
      , "ter"
      ]

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 12, 4, 30, 0) Second)
             [ "agora"
             , "já"
             , "ja"
             , "nesse instante"
             , "neste instante"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "hoje"
             , "nesse momento"
             , "neste momento"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "ontem"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "antes de ontem"
             , "anteontem"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "amanhã"
             , "amanha"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "depois de amanhã"
             , "depois de amanha"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "segunda-feira"
             , "segunda feira"
             , "segunda"
             , "seg."
             , "seg"
             , "essa segunda-feira"
             , "essa segunda feira"
             , "essa segunda"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "segunda, 18 de fevereiro"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "terça-feira"
             , "terça feira"
             , "terça"
             , "terca-feira"
             , "terca feira"
             , "terca"
             , "ter."
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "quarta-feira"
             , "quarta feira"
             , "quarta"
             , "qua."
             , "qua"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "quinta-feira"
             , "quinta feira"
             , "quinta"
             , "qui."
             , "qui"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "sexta-feira"
             , "sexta feira"
             , "sexta"
             , "sex."
             , "sex"
             ]
  , examples (datetime (2013, 2, 16, 0, 0, 0) Day)
             [ "sábado"
             , "sabado"
             , "sáb."
             , "sáb"
             , "sab."
             , "sab"
             ]
  , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
             [ "domingo"
             , "dom."
             , "dom"
             ]
  , examples (datetime (2013, 5, 5, 0, 0, 0) Day)
             [ "5 de maio"
             , "cinco de maio"
             ]
  , examples (datetime (2013, 5, 5, 0, 0, 0) Day)
             [ "cinco de maio de 2013"
             , "5 de maio de 2013"
             , "5/5"
             , "5/5/2013"
             ]
  , examples (datetime (2013, 7, 4, 0, 0, 0) Day)
             [ "4 de julho"
             , "quatro de julho"
             , "4/7"
             , "4/7/2013"
             ]
  , examples (datetime (2013, 3, 3, 0, 0, 0) Day)
             [ "3 de março"
             , "três de março"
             , "tres de março"
             , "3/3"
             , "3/3/2013"
             ]
  , examples (datetime (2013, 4, 5, 0, 0, 0) Day)
             [ "5 de abril"
             , "cinco de abril"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "1 de março"
             , "primeiro de março"
             , "um de março"
             , "1o de março"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "1-3-2013"
             , "1.3.2013"
             , "1/3/2013"
             ]
  , examples (datetime (2013, 2, 16, 0, 0, 0) Day)
             [ "essa dia 16"
             , "16 de fevereiro"
             ]
  , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
             [ "este dia 17"
             , "17 de fevereiro"
             , "17/2"
             , "no domingo"
             , "no dia 17"
             ]
  , examples (datetime (2013, 2, 20, 0, 0, 0) Day)
             [ "esse dia 20"
             , "20 de fevereiro"
             , "20/2"
             ]
  , examples (datetime (1974, 10, 31, 0, 0, 0) Day)
             [ "31/10/1974"
             , "31/10/74"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "próxima terça-feira"
             , "próxima terça feira"
             , "próxima terça"
             , "proxima terça-feira"
             , "proxima terça feira"
             , "proxima terça"
             ]
  , examples (datetime (2013, 2, 20, 0, 0, 0) Day)
             [ "quarta que vem"
             , "quarta da semana que vem"
             , "quarta da próxima semana"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "terça desta semana"
             , "terça dessa semana"
             , "terça agora"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Week)
             [ "esta semana"
             ]
  , examples (datetime (2013, 2, 4, 0, 0, 0) Week)
             [ "semana passada"
             , "semana anterior"
             , "passada semana"
             , "anterior semana"
             , "última semana"
             , "ultima semana"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Week)
             [ "semana que vem"
             , "proxima semana"
             ]
  , examples (datetime (2013, 1, 0, 0, 0, 0) Month)
             [ "mês passado"
             ]
  , examples (datetime (2013, 3, 0, 0, 0, 0) Month)
             [ "mes que vem"
             , "próximo mês"
             ]
  , examples (datetime (2012, 0, 0, 0, 0, 0) Year)
             [ "ano passado"
             ]
  , examples (datetime (2013, 0, 0, 0, 0, 0) Year)
             [ "este ano"
             ]
  , examples (datetime (2014, 0, 0, 0, 0, 0) Year)
             [ "ano que vem"
             , "proximo ano"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "domingo passado"
             , "domingo da semana passada"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "terça passada"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "às tres da tarde"
             , "às tres"
             , "às 3 pm"
             , "às 15 horas"
             ]
  , examples (datetime (2013, 2, 12, 20, 0, 0) Hour)
             [ "às oito da noite"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Minute)
             [ "15:00"
             , "15.00"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Hour)
             [ "meianoite"
             , "meia noite"
             ]
  , examples (datetime (2013, 2, 12, 12, 0, 0) Hour)
             [ "meio dia"
             , "meiodia"
             ]
  , examples (datetime (2013, 2, 12, 12, 15, 0) Minute)
             [ "meio dia e quinze"
             ]
  , examples (datetime (2013, 2, 12, 11, 55, 0) Minute)
             [ "5 para meio dia"
             ]
  , examples (datetime (2013, 2, 12, 12, 30, 0) Minute)
             [ "meio dia e meia"
             ]
  , examples (datetime (2013, 2, 12, 6, 0, 0) Hour)
             [ "as seis da manha"
             , "as seis pela manha"
             ]
  , examples (datetime (2013, 2, 12, 15, 15, 0) Minute)
             [ "às tres e quinze"
             , "às tres e quinze da tarde"
             , "às tres e quinze pela tarde"
             , "15:15"
             , "15.15"
             ]
  , examples (datetime (2013, 2, 12, 15, 30, 0) Minute)
             [ "às tres e meia"
             , "às 3 e trinta"
             , "às tres e meia da tarde"
             , "às 3 e trinta da tarde"
             , "15:30"
             , "15.30"
             ]
  , examples (datetime (2013, 2, 12, 11, 45, 0) Minute)
             [ "quinze para meio dia"
             , "quinze para o meio dia"
             , "11:45"
             , "as onze e quarenta e cinco"
             , "hoje quinze para o meio dia"
             , "hoje às onze e quarenta e cinco"
             ]
  , examples (datetime (2013, 2, 12, 5, 15, 0) Minute)
             [ "5 e quinze"
             ]
  , examples (datetime (2013, 2, 12, 6, 0, 0) Hour)
             [ "6 da manhã"
             ]
  , examples (datetime (2013, 2, 13, 11, 0, 0) Hour)
             [ "quarta às onze da manhã"
             ]
  , examples (datetime (2014, 9, 12, 0, 0, 0) Day)
             [ "sexta, 12 de setembro de 2014"
             , "sexta feira, 12 de setembro de 2014"
             , "12 de setembro de 2014, sexta"
             , "12 de setembro de 2014 sexta feira"
             , "sexta feira 12 de setembro de 2014"
             ]
  , examples (datetime (2013, 2, 12, 4, 30, 1) Second)
             [ "em um segundo"
             ]
  , examples (datetime (2013, 2, 12, 4, 31, 0) Second)
             [ "em um minuto"
             , "em 1 min"
             ]
  , examples (datetime (2013, 2, 12, 4, 32, 0) Second)
             [ "em 2 minutos"
             , "em dois minutos"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Second)
             [ "em 60 minutos"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Minute)
             [ "em uma hora"
             ]
  , examples (datetime (2013, 2, 12, 2, 30, 0) Minute)
             [ "fazem duas horas"
             ]
  , examples (datetime (2013, 2, 13, 4, 30, 0) Minute)
             [ "em 24 horas"
             , "em vinte e quatro horas"
             ]
  , examples (datetime (2013, 2, 13, 4, 0, 0) Hour)
             [ "em um dia"
             ]
  , examples (datetime (2013, 2, 19, 4, 0, 0) Hour)
             [ "em 7 dias"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "em uma semana"
             ]
  , examples (datetime (2013, 1, 22, 0, 0, 0) Day)
             [ "faz tres semanas"
             , "faz três semanas"
             ]
  , examples (datetime (2013, 4, 12, 0, 0, 0) Day)
             [ "em dois meses"
             ]
  , examples (datetime (2012, 11, 12, 0, 0, 0) Day)
             [ "faz tres meses"
             ]
  , examples (datetime (2014, 2, 0, 0, 0, 0) Month)
             [ "em um ano"
             , "em 1 ano"
             ]
  , examples (datetime (2011, 2, 0, 0, 0, 0) Month)
             [ "faz dois anos"
             ]
  , examples (datetimeInterval ((2013, 6, 21, 0, 0, 0), (2013, 9, 24, 0, 0, 0)) Day)
             [ "este verão"
             , "este verao"
             ]
  , examples (datetimeInterval ((2012, 12, 21, 0, 0, 0), (2013, 3, 21, 0, 0, 0)) Day)
             [ "este inverno"
             ]
  , examples (datetime (2013, 12, 25, 0, 0, 0) Day)
             [ "Natal"
             ]
  , examples (datetime (2013, 12, 31, 0, 0, 0) Day)
             [ "véspera de ano novo"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Day)
             [ "ano novo"
             , "reveillon"
             , "Reveillon"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 18, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
             [ "esta noite"
             , "essa noite"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 18, 0, 0), (2013, 2, 14, 0, 0, 0)) Hour)
             [ "amanhã a noite"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 18, 0, 0), (2013, 2, 12, 0, 0, 0)) Hour)
             [ "ontem a noite"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 18, 0, 0), (2013, 2, 18, 0, 0, 0)) Hour)
             [ "este final de semana"
             , "este fim de semana"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 4, 0, 0), (2013, 2, 18, 12, 0, 0)) Hour)
             [ "segunda de manhã"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 4, 0, 0), (2013, 2, 15, 12, 0, 0)) Hour)
             [ "dia 15 de fevereiro pela manhã"
             , "dia 15 de fevereiro de manhã"
             ]
  , examples (datetime (2013, 2, 12, 20, 0, 0) Hour)
             [ "às 8 da noite"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 29, 58), (2013, 2, 12, 4, 30, 0)) Second)
             [ "2 segundos atras"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 1), (2013, 2, 12, 4, 30, 4)) Second)
             [ "proximos 3 segundos"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 28, 0), (2013, 2, 12, 4, 30, 0)) Minute)
             [ "2 minutos atrás"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 31, 0), (2013, 2, 12, 4, 34, 0)) Minute)
             [ "proximos 3 minutos"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 5, 0, 0), (2013, 2, 12, 8, 0, 0)) Hour)
             [ "proximas 3 horas"
             ]
  , examples (datetimeInterval ((2013, 2, 10, 0, 0, 0), (2013, 2, 12, 0, 0, 0)) Day)
             [ "passados 2 dias"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 16, 0, 0, 0)) Day)
             [ "proximos 3 dias"
             ]
  , examples (datetimeInterval ((2013, 1, 28, 0, 0, 0), (2013, 2, 11, 0, 0, 0)) Week)
             [ "duas semanas atras"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 0, 0, 0), (2013, 3, 11, 0, 0, 0)) Week)
             [ "3 proximas semanas"
             , "3 semanas que vem"
             ]
  , examples (datetimeInterval ((2012, 12, 0, 0, 0, 0), (2013, 2, 0, 0, 0, 0)) Month)
             [ "passados 2 meses"
             , "últimos 2 meses"
             , "2 meses anteriores"
             , "2 últimos meses"
             , "2 anteriores meses"
             ]
  , examples (datetimeInterval ((2013, 3, 0, 0, 0, 0), (2013, 6, 0, 0, 0, 0)) Month)
             [ "3 próximos meses"
             , "proximos tres meses"
             , "tres meses que vem"
             ]
  , examples (datetimeInterval ((2011, 0, 0, 0, 0, 0), (2013, 0, 0, 0, 0, 0)) Year)
             [ "passados 2 anos"
              , "últimos 2 anos"
              , "2 anos anteriores"
              , "2 últimos anos"
              , "2 anteriores anos"
             ]
  , examples (datetimeInterval ((2014, 0, 0, 0, 0, 0), (2017, 0, 0, 0, 0, 0)) Year)
             [ "3 próximos anos"
             , "proximo tres anos"
             , "3 anos que vem"
             ]
  , examples (datetimeInterval ((2013, 7, 13, 0, 0, 0), (2013, 7, 16, 0, 0, 0)) Day)
             [ "13 a 15 de julho"
             , "13 - 15 de julho de 2013"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 9, 30, 0), (2013, 2, 12, 11, 1, 0)) Minute)
             [ "9:30 - 11:00"
             ]
  , examples (datetimeInterval ((2013, 12, 21, 0, 0, 0), (2014, 1, 7, 0, 0, 0)) Day)
             [ "21 de Dez. a 6 de Jan"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 2, 12, 7, 30, 0)) Second)
             [ "dentro de tres horas"
             ]
  , examples (datetime (2013, 2, 12, 16, 0, 0) Hour)
             [ "as quatro da tarde"
             ]
  , examples (datetime (2013, 2, 12, 13, 0, 0) Minute)
             [ "as quatro CET"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 12, 12, 0, 0) Hour)
             [ "após ao meio dia"
             , "depois do meio dia"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 12, 12, 0, 0) Hour)
             [ "antes do meio dia"
             , "não mais que meio dia"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 13, 15, 0, 0) Hour)
             [ "amanhã depois das 15hs"
             , "amanha após as quinze horas"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 13, 0, 0, 0) Hour)
             [ "antes da meia noite"
             , "até a meia noite"
             ]
 ,examples (datetime (2013, 2, 12, 3, 0, 0) Hour)
             [ "última hora"
             , "hora anterior"
             , "hora passada"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Quarter)
             [ "este trimestre"
             , "trimestre actual"
             , "trimestre atual"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Month)
             [ "primeiro mês de 2013"
             , "primeiro mês 2013"
             ]
  , examples (datetime (2013, 4, 1, 0, 0, 0) Quarter)
             [ "próximo trimestre"
             , "segundo trimestre de 2013"
             , "segundo trimestre"
             ]
  , examples (datetime (2013, 7, 1, 0, 0, 0) Quarter)
             [ "terceiro trimestre"
             ]
  , examples (datetime (2018, 10, 1, 0, 0, 0) Quarter)
             [ "quarto trimestre de 2018"
             , "quarto trimestre 2018"
             ]
  , examples (datetime (2012, 10, 1, 0, 0, 0) Quarter)
             [ "trimestre passado"
             , "trimestre anterior"
             , "último trimestre"
             , "ultimo trimestre"
             ]
  , examples (datetime (2013, 12, 1, 0, 0, 0) Month)
             [ "décimo segundo mês de 2013"
             , "último mês de 2013"
             , "último mês 2013"
             ]
  , examples (datetime (2015, 10, 1, 0, 0, 0) Quarter)
             [ "último trimestre de 2015"
             , "último trimestre 2015"
             ]
  , examples (datetimeInterval ((2013, 7, 13, 0, 0, 0), (2013, 7, 16, 0, 0, 0)) Day)
             [ "desde 13 a 15 de Julho"
             , "a partir de 13 até 15 de Julho"
             , "desde 13 até 15 de Julho"
             , "13-15 de Julho"
             , "13 até 15 de Julho"
             , "13 a 15 de Julho"
             , "desde 13 a 15 Julho"
             , "a partir de 13 até 15 Julho"
             , "desde 13 até 15 Julho"
             , "13-15 Julho"
             , "13 até 15 Julho"
             , "13 a 15 Julho"
             ]
  , examples (datetimeInterval ((2017, 1, 1, 0, 0, 0), (2017, 10, 1, 0, 0, 0)) Quarter)
             [ "de primeiro trimestre de 2017 até terceiro trimestre de 2017"
             , "de primeiro trimestre de 2017 até ao terceiro trimestre de 2017"
             , "do primeiro trimestre de 2017 até terceiro trimestre de 2017"
             , "do primeiro trimestre de 2017 até ao terceiro trimestre de 2017"
             , "desde primeiro trimestre de 2017 até terceiro trimestre de 2017"
             , "desde primeiro trimestre de 2017 até ao terceiro trimestre de 2017"
             , "a partir do primeiro trimestre de 2017 até terceiro trimestre de 2017"
             , "a partir do primeiro trimestre de 2017 até ao terceiro trimestre de 2017"
             , "a partir de primeiro trimestre de 2017 até terceiro trimestre de 2017"
             , "a partir de primeiro trimestre de 2017 até ao terceiro trimestre de 2017"
             , "primeiro trimestre de 2017 a terceiro trimestre de 2017"
             , "primeiro trimestre de 2017 ao terceiro trimestre de 2017"
             , "primeiro trimestre de 2017 - terceiro trimestre de 2017"
             , "entre primeiro trimestre de 2017 e terceiro trimestre de 2017"
             , "entre o primeiro trimestre de 2017 e terceiro trimestre de 2017"
             , "primeiro trimestre de 2017 até terceiro trimestre de 2017"
             , "primeiro trimestre de 2017 até ao terceiro trimestre de 2017"
             , "primeiro trimestre 2017 a terceiro trimestre 2017"
             , "primeiro trimestre 2017 ao terceiro trimestre 2017"
             , "primeiro trimestre 2017 - terceiro trimestre 2017"
             , "primeiro trimestre 2017 até terceiro trimestre 2017"
             , "primeiro trimestre 2017 até ao terceiro trimestre 2017"
             , "entre primeiro trimestre 2017 e terceiro trimestre 2017"
             , "entre o primeiro trimestre 2017 e terceiro trimestre 2017"
             ]
  , examples (datetimeInterval ((2017, 3, 1, 0, 0, 0), (2017, 10, 1, 0, 0, 0)) Month)
              [ "de terceiro mês de 2017 até nono mês de 2017"
              , "de terceiro mês de 2017 até ao nono mês de 2017"
              , "do terceiro mês de 2017 até nono mês de 2017"
              , "do terceiro mês de 2017 até ao nono mês de 2017"
              , "desde terceiro mês de 2017 até nono mês de 2017"
              , "desde terceiro mês de 2017 até ao nono mês de 2017"
              , "a partir do terceiro mês de 2017 até nono mês de 2017"
              , "a partir do terceiro mês de 2017 até ao nono mês de 2017"
              , "a partir de terceiro mês de 2017 até nono mês de 2017"
              , "a partir de terceiro mês de 2017 até ao nono mês de 2017"
              , "terceiro mês de 2017 a nono mês de 2017"
              , "terceiro mês de 2017 ao nono mês de 2017"
              , "terceiro mês de 2017 - nono mês de 2017"
              , "entre terceiro mês de 2017 e nono mês de 2017"
              , "entre o terceiro mês de 2017 e nono mês de 2017"
              , "terceiro mês de 2017 até nono mês de 2017"
              , "terceiro mês de 2017 até ao nono mês de 2017"
              , "terceiro mês 2017 a nono mês 2017"
              , "terceiro mês 2017 ao nono mês 2017"
              , "terceiro mês 2017 - nono mês 2017"
              , "terceiro mês 2017 até nono mês 2017"
              , "terceiro mês 2017 até ao nono mês 2017"
              , "entre terceiro mês 2017 e nono mês 2017"
              , "entre o terceiro mês 2017 e nono mês 2017"
              ]
  ]
