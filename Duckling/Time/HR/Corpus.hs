-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.HR.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Time.Corpus
import Duckling.Time.Types hiding (Month)
import Duckling.TimeGrain.Types hiding (add)
import Duckling.Testing.Types hiding (examples)

corpus :: Corpus
corpus = (testContext {locale = makeLocale HR Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 12, 4, 30, 0) Second)
             [ "sad"
             , "sada"
             , "upravo sad"
             , "ovaj tren"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "danas"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "jucer"
             , "jučer"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "sutra"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "ponedjeljak"
             , "pon."
             , "ovaj ponedjeljak"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "ponedjeljak, 18. veljace"
             , "ponedjeljak, 18. veljače"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "utorak"
             , "utorak 19."
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "cetvrtak"
             , "četvrtak"
             , "čet"
             , "cet."
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "petak"
             , "pet"
             , "pet."
             ]
  , examples (datetime (2013, 2, 16, 0, 0, 0) Day)
             [ "subota"
             , "sub"
             , "sub."
             ]
  , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
             [ "nedjelja"
             , "ned"
             , "ned."
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "1. ozujak"
             , "1. ožujak"
             , "prvi ozujka"
             ]
  , examples (datetime (2013, 3, 3, 0, 0, 0) Day)
             [ "treci ozujka"
             , "treci ožujka"
             ]
  , examples (datetime (2015, 3, 3, 0, 0, 0) Day)
             [ "3. ozujka 2015"
             , "treci ozujka 2015"
             , "3/3/2015"
             , "3/3/15"
             , "2015-3-3"
             , "2015-03-03"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "15ti drugi"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "15. veljace"
             , "15. veljače"
             , "15/02"
             ]
  , examples (datetime (2013, 8, 8, 0, 0, 0) Day)
             [ "8. kolovoza"
             , "8. kolovoz"
             ]
  , examples (datetime (2014, 10, 0, 0, 0, 0) Month)
             [ "listopad 2014"
             ]
  , examples (datetime (1974, 10, 31, 0, 0, 0) Day)
             [ "31/10/1974"
             , "31/10/74"
             , "74-10-31"
             ]
  , examples (datetime (2015, 4, 14, 0, 0, 0) Day)
             [ "14travanj 2015"
             , "14. travnja, 2015"
             , "14. travanj 15"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "sljedeci utorak"
             , "sljedeceg utorka"
             ]
  , examples (datetime (2013, 2, 22, 0, 0, 0) Day)
             [ "petak nakon sljedeceg"
             ]
  , examples (datetime (2013, 3, 0, 0, 0, 0) Month)
             [ "sljedeci ozujak"
             ]
  , examples (datetime (2014, 3, 0, 0, 0, 0) Month)
             [ "ozujak nakon sljedeceg"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "nedjelja, 10. veljace"
             , "nedjelja, 10. veljače"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "Sri, 13. velj"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "ponedjeljak, veljaca 18."
             , "Pon, 18. veljace"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Week)
             [ "ovaj tjedan"
             ]
  , examples (datetime (2013, 2, 4, 0, 0, 0) Week)
             [ "prosli tjedan"
             , "prošli tjedan"
             , "prethodni tjedan"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Week)
             [ "sljedeci tjedan"
             ]
  , examples (datetime (2013, 1, 0, 0, 0, 0) Month)
             [ "prethodni mjesec"
             ]
  , examples (datetime (2013, 3, 0, 0, 0, 0) Month)
             [ "sljedeci mjesec"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Quarter)
             [ "ovaj kvartal"
             , "ovo tromjesecje"
             ]
  , examples (datetime (2013, 4, 1, 0, 0, 0) Quarter)
             [ "sljedeci kvartal"
             ]
  , examples (datetime (2013, 7, 1, 0, 0, 0) Quarter)
             [ "treci kvartal"
             , "3. kvartal"
             , "trece tromjesecje"
             , "3. tromjesečje"
             ]
  , examples (datetime (2018, 10, 1, 0, 0, 0) Quarter)
             [ "4. kvartal 2018"
             , "četvrto tromjesečje 2018"
             ]
  , examples (datetime (2012, 0, 0, 0, 0, 0) Year)
             [ "prošla godina"
             , "prethodna godina"
             ]
  , examples (datetime (2013, 0, 0, 0, 0, 0) Year)
             [ "ova godina"
             ]
  , examples (datetime (2014, 0, 0, 0, 0, 0) Year)
             [ "sljedece godina"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "prosle nedjelje"
             , "prosli tjedan u nedjelju"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "prosli utorak"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "sljedeci utorak"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "sljedecu srijedu"
             ]
  , examples (datetime (2013, 2, 20, 0, 0, 0) Day)
             [ "sljedeci tjedan u srijedu"
             , "srijeda sljedeci tjedan"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "sljedeci petak"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "ovaj tjedan u ponedjeljak"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "ovaj utorak"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "ova srijeda"
             , "ovaj tjedan u srijedu"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "prekosutra"
             ]
  , examples (datetime (2013, 2, 14, 17, 0, 0) Hour)
             [ "prekosutra u 5 popodne"
             , "prekosutra u 17"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "prekjucer"
             , "prekjučer"
             ]
  , examples (datetime (2013, 2, 10, 8, 0, 0) Hour)
             [ "prekjučer u 8"
             , "prekjučer u 8 sati"
             ]
  , examples (datetime (2013, 3, 25, 0, 0, 0) Day)
             [ "zadnji ponedjeljak u ozujku"
             ]
  , examples (datetime (2014, 3, 30, 0, 0, 0) Day)
             [ "zadnja nedjelja u ozujku 2014"
             ]
  , examples (datetime (2013, 10, 3, 0, 0, 0) Day)
             [ "treci dan u listopadu"
             ]
  , examples (datetime (2014, 10, 6, 0, 0, 0) Week)
             [ "prvi tjedan u listopadu 2014"
             ]
  , examples (datetime (2015, 10, 31, 0, 0, 0) Day)
             [ "zadnji dan u listopadu 2015"
             ]
  , examples (datetime (2014, 9, 22, 0, 0, 0) Week)
             [ "zadnji tjedan u rujnu 2014"
             ]
  , examples (datetime (2013, 10, 1, 0, 0, 0) Day)
             [ "prvi utorak u listopadu"
             ]
  , examples (datetime (2014, 9, 16, 0, 0, 0) Day)
             [ "treci utorak u rujnu 2014"
             ]
  , examples (datetime (2014, 10, 1, 0, 0, 0) Day)
             [ "prva srijeda u listopadu 2014"
             ]
  , examples (datetime (2014, 10, 8, 0, 0, 0) Day)
             [ "druga srijeda u listopadu 2014"
             ]
  , examples (datetime (2015, 1, 13, 0, 0, 0) Day)
             [ "treci utorak poslije Bozica 2014"
             ]
  , examples (datetime (2013, 2, 13, 3, 0, 0) Hour)
             [ "3 u noci"
             , "u 3 ujutro"
             , "u tri sata u noci"
             ]
  , examples (datetime (2013, 2, 12, 3, 18, 0) Minute)
             [ "3:18 rano"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "u 3 poslijepodne"
             , "@ 15"
             , "15 sati poslijepodne"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "oko 3 poslijepodne"
             , "otprilike u 3 poslijepodne"
             , "cca 3 poslijepodne"
             , "cca 15"
             ]
  , examples (datetime (2013, 2, 12, 15, 15, 0) Minute)
             [ "15 i 15"
             , "3:15 poslijepodne"
             , "15:15"
             ]
  , examples (datetime (2013, 2, 12, 15, 15, 0) Minute)
             [ "cetvrt nakon 3 poslijepodne"
             ]
  , examples (datetime (2013, 2, 12, 15, 20, 0) Minute)
             [ "3 i 20 popodne"
             , "3:20 poslijepodne"
             , "3:20 popodne"
             , "dvadeset nakon 3 popodne"
             , "15:20"
             ]
  , examples (datetime (2013, 2, 12, 15, 30, 0) Minute)
             [ "tri i po popodne"
             , "pola 4 popodne"
             , "15:30"
             , "pola cetiri popodne"
             ]
  , examples (datetime (2013, 2, 12, 15, 23, 24) Second)
             [ "15:23:24"
             ]
  , examples (datetime (2013, 2, 12, 11, 45, 0) Minute)
             [ "petnaest do podne"
             , "11:45"
             , "četvrt do podneva"
             ]
  , examples (datetime (2013, 2, 12, 20, 0, 0) Hour)
             [ "8 navecer"
             , "osam sati navecer"
             , "danas 8 navecer"
             ]
  , examples (datetime (2013, 9, 20, 19, 30, 0) Minute)
             [ "u 7:30 popodne u pet, 20. rujna"
             ]
  , examples (datetime (2013, 2, 16, 9, 0, 0) Hour)
             [ "9 ujutro u subotu"
             , "u subotu u 9 sati ujutro"
             ]
  , examples (datetime (2014, 7, 18, 19, 0, 0) Minute)
             [ "pet, srp 18., 2014, 19:00"
             , "pet, srp 18., 2014 u 19:00"
             ]
  , examples (datetime (2013, 2, 12, 4, 30, 1) Second)
             [ "za jednu sekundu"
             ]
  , examples (datetime (2013, 2, 12, 4, 31, 0) Second)
             [ "za jednu minutu"
             ]
  , examples (datetime (2013, 2, 12, 4, 32, 0) Second)
             [ "za 2 minute"
             , "za jos 2 minute"
             , "2 minute od sad"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Second)
             [ "za 60 minuta"
             ]
  , examples (datetime (2013, 2, 12, 4, 45, 0) Second)
             [ "oko cetvrt sata"
             , "oko 1/4h"
             , "oko 1/4 h"
             , "oko 1/4 sata"
             ]
  , examples (datetime (2013, 2, 12, 5, 0, 0) Second)
             [ "za pola sata"
             , "za pol sata"
             , "za 1/2h"
             , "za 1/2 h"
             , "za 1/2 sata"
             ]
  , examples (datetime (2013, 2, 12, 5, 15, 0) Second)
             [ "za tri-cetvrt sata"
             , "za 3/4h"
             , "za 3/4 h"
             , "za 3/4 sata"
             ]
  , examples (datetime (2013, 2, 12, 7, 0, 0) Second)
             [ "za 2.5 sata"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Minute)
             [ "za jedan sat"
             , "za 1h"
             ]
  , examples (datetime (2013, 2, 12, 6, 30, 0) Minute)
             [ "za par sati"
             ]
  , examples (datetime (2013, 2, 12, 7, 30, 0) Minute)
             [ "za nekoliko sati"
             ]
  , examples (datetime (2013, 2, 13, 4, 30, 0) Minute)
             [ "za 24 sata"
             , "za 24h"
             ]
  , examples (datetime (2013, 2, 13, 4, 0, 0) Hour)
             [ "za 1 dan"
             , "za jedan dan"
             ]
  , examples (datetime (2016, 2, 0, 0, 0, 0) Month)
             [ "3 godine od danasnjeg dana"
             ]
  , examples (datetime (2013, 2, 19, 4, 0, 0) Hour)
             [ "za 7 dana"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "za 1 tjedan"
             ]
  , examples (datetime (2013, 2, 12, 5, 0, 0) Second)
             [ "za oko pola sata"
             ]
  , examples (datetime (2013, 2, 5, 4, 0, 0) Hour)
             [ "prije 7 dana"
             ]
  , examples (datetime (2013, 1, 29, 4, 0, 0) Hour)
             [ "prije 14 dana"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "prije jedan tjedan"
             , "prije jednog tjedna"
             ]
  , examples (datetime (2013, 1, 22, 0, 0, 0) Day)
             [ "prije tri tjedna"
             ]
  , examples (datetime (2012, 11, 12, 0, 0, 0) Day)
             [ "prije tri mjeseca"
             ]
  , examples (datetime (2011, 2, 0, 0, 0, 0) Month)
             [ "prije dvije godine"
             ]
  , examples (datetime (1954, 0, 0, 0, 0, 0) Year)
             [ "1954"
             ]
  , examples (datetime (2013, 2, 19, 4, 0, 0) Hour)
             [ "za 7 dana"
             ]
  , examples (datetime (2013, 2, 26, 4, 0, 0) Hour)
             [ "za 14 dana"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "za jedan tjedan"
             ]
  , examples (datetime (2013, 3, 5, 0, 0, 0) Day)
             [ "za tri tjedna"
             ]
  , examples (datetime (2013, 5, 12, 0, 0, 0) Day)
             [ "za tri mjeseca"
             ]
  , examples (datetime (2015, 2, 0, 0, 0, 0) Month)
             [ "za dvije godine"
             ]
  , examples (datetime (2013, 12, 25, 0, 0, 0) Day)
             [ "jednu godinu poslije Bozica"
             ]
  , examples (datetimeInterval ((2013, 6, 21, 0, 0, 0), (2013, 9, 24, 0, 0, 0)) Day)
             [ "ovog ljeta"
             , "ovo ljeto"
             , "ljetos"
             ]
  , examples (datetimeInterval ((2012, 12, 21, 0, 0, 0), (2013, 3, 21, 0, 0, 0)) Day)
             [ "ove zime"
             , "zimus"
             ]
  , examples (datetime (2013, 12, 25, 0, 0, 0) Day)
             [ "Bozic"
             , "zicbo"
             ]
  , examples (datetime (2013, 12, 31, 0, 0, 0) Day)
             [ "stara godina"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Day)
             [ "nova godina"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "valentinovo"
             ]
  , examples (datetime (2013, 5, 12, 0, 0, 0) Day)
             [ "majcin dan"
             ]
  , examples (datetime (2013, 6, 16, 0, 0, 0) Day)
             [ "dan oceva"
             ]
  , examples (datetime (2013, 10, 31, 0, 0, 0) Day)
             [ "noc vjestica"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 18, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
             [ "veceras"
             , "ove veceri"
             , "danas navecer"
             ]
  , examples (datetimeInterval ((2013, 2, 8, 18, 0, 0), (2013, 2, 11, 0, 0, 0)) Hour)
             [ "prosli vikend"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 18, 0, 0), (2013, 2, 14, 0, 0, 0)) Hour)
             [ "sutra navecer"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 12, 0, 0), (2013, 2, 13, 14, 0, 0)) Hour)
             [ "sutra rucak"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 18, 0, 0), (2013, 2, 12, 0, 0, 0)) Hour)
             [ "jucer navecer"
             , "prethodne veceri"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 18, 0, 0), (2013, 2, 18, 0, 0, 0)) Hour)
             [ "ovaj vikend"
             , "ovog vikenda"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 4, 0, 0), (2013, 2, 18, 12, 0, 0)) Hour)
             [ "ponedjeljak ujutro"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 3, 0, 0), (2013, 2, 18, 9, 0, 0)) Hour)
             [ "ponedjeljak rano ujutro"
             , "ponedjeljak rano"
             , "ponedjeljak u rane jutarnje sate"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 4, 0, 0), (2013, 2, 15, 12, 0, 0)) Hour)
             [ "15. veljace ujutro"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 29, 58), (2013, 2, 12, 4, 30, 0)) Second)
             [ "prosle 2 sekunde"
             , "prethodne dvije sekunde"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 1), (2013, 2, 12, 4, 30, 4)) Second)
             [ "sljedece 3 sekunde"
             , "sljedece tri sekunde"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 28, 0), (2013, 2, 12, 4, 30, 0)) Minute)
             [ "prosle 2 minute"
             , "prethodne dvije minute"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 31, 0), (2013, 2, 12, 4, 34, 0)) Minute)
             [ "sljedece 3 minute"
             , "sljedece tri minute"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 3, 0, 0), (2013, 2, 12, 4, 0, 0)) Hour)
             [ "prethodni jedan sat"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 4, 0, 0), (2013, 2, 12, 4, 0, 0)) Hour)
             [ "prethodna 24 sata"
             , "prethodna dvadeset i cetiri sata"
             , "prethodna dvadeset i cetiri sata"
             , "prethodna 24h"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 5, 0, 0), (2013, 2, 12, 8, 0, 0)) Hour)
             [ "sljedeca 3 sata"
             , "sljedeca tri sata"
             ]
  , examples (datetimeInterval ((2013, 2, 10, 0, 0, 0), (2013, 2, 12, 0, 0, 0)) Day)
             [ "prethodna dva dana"
             , "prethodna 2 dana"
             , "prosla 2 dana"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 16, 0, 0, 0)) Day)
             [ "sljedeca 3 dana"
             , "sljedeca tri dana"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 16, 0, 0, 0)) Day)
             [ "sljedecih nekoliko dana"
             ]
  , examples (datetimeInterval ((2013, 1, 28, 0, 0, 0), (2013, 2, 11, 0, 0, 0)) Week)
             [ "prethodna 2 tjedna"
             , "prethodna dva tjedna"
             , "prosla 2 tjedna"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 0, 0, 0), (2013, 3, 11, 0, 0, 0)) Week)
             [ "sljedeca 3 tjedna"
             , "sljedeca tri tjedna"
             ]
  , examples (datetimeInterval ((2012, 12, 0, 0, 0, 0), (2013, 2, 0, 0, 0, 0)) Month)
             [ "prethodna 2 mjeseca"
             , "prethodna dva mjeseca"
             ]
  , examples (datetimeInterval ((2013, 3, 0, 0, 0, 0), (2013, 6, 0, 0, 0, 0)) Month)
             [ "sljedeca 3 mjeseca"
             , "sljedeca tri mjeseca"
             ]
  , examples (datetimeInterval ((2011, 0, 0, 0, 0, 0), (2013, 0, 0, 0, 0, 0)) Year)
             [ "prethodne 2 godine"
             , "prethodne dvije godine"
             ]
  , examples (datetimeInterval ((2014, 0, 0, 0, 0, 0), (2017, 0, 0, 0, 0, 0)) Year)
             [ "sljedece 3 godine"
             , "sljedece tri godine"
             ]
  , examples (datetimeInterval ((2013, 7, 13, 0, 0, 0), (2013, 7, 16, 0, 0, 0)) Day)
             [ "srpanj 13-15"
             , "srpanj 13 do 15"
             , "srpanj 13 - srpanj 15"
             ]
  , examples (datetimeInterval ((2013, 8, 8, 0, 0, 0), (2013, 8, 13, 0, 0, 0)) Day)
             [ "kol 8 - kol 12"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 9, 30, 0), (2013, 2, 12, 11, 1, 0)) Minute)
             [ "9:30 - 11:00"
             ]
  , examples (datetimeInterval ((2013, 2, 14, 9, 30, 0), (2013, 2, 14, 11, 1, 0)) Minute)
             [ "od 9:30 - 11:00 u cetvrtak"
             , "između 9:30 i 11:00 u cetvrtak"
             , "9:30 - 11:00 u cetvrtak"
             , "izmedju 9:30 i 11:00 u cetvrtak"
             , "cetvrtak od 9:30 do 11:00"
             , "od 9:30 do 11:00 u cetvrtak"
             , "cetvrtak od 9:30 do 11:00"
             ]
  , examples (datetimeInterval ((2013, 2, 14, 9, 0, 0), (2013, 2, 14, 12, 0, 0)) Hour)
             [ "cetvrtak od 9 do 11 ujutro"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 11, 30, 0), (2013, 2, 12, 13, 31, 0)) Minute)
             [ "11:30-1:30"
             ]
  , examples (datetime (2013, 9, 21, 13, 30, 0) Minute)
             [ "1:30 poslijepodne u sub, ruj 21."
             ]
  , examples (datetimeInterval ((2013, 2, 18, 0, 0, 0), (2013, 3, 4, 0, 0, 0)) Week)
             [ "sljedeca 2 tjedna"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 12, 14, 0, 0) Hour)
             [ "nekad do 2 poslijepodne"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 2, 13, 0, 0, 0)) Second)
             [ "do kraja ovog dana"
             , "do kraja dana"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 3, 1, 0, 0, 0)) Second)
             [ "do kraja ovog mjeseca"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 4, 1, 0, 0, 0)) Second)
             [ "do kraja sljedeceg mjeseca"
             ]
  , examples (datetime (2013, 2, 12, 13, 0, 0) Minute)
             [ "4 poslijepodne CET"
             ]
  , examples (datetime (2013, 2, 14, 6, 0, 0) Minute)
             [ "cetvrtak 8:00 GMT"
             , "cetvrtak 8:00 gmt"
             ]
  , examples (datetime (2013, 2, 12, 14, 0, 0) Hour)
             [ "danas u 14"
             , "u 2 poslijepodne"
             ]
  , examples (datetime (2013, 4, 25, 16, 0, 0) Hour)
             [ "25/4 U 16 sati"
             ]
  , examples (datetime (2013, 2, 13, 15, 0, 0) Hour)
             [ "15 sati sutra"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 17, 4, 0, 0) Hour)
             [ "nakon 5 dana"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 12, 11, 0, 0) Hour)
             [ "prije 11 sat"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 12, 0, 0), (2013, 2, 12, 20, 0, 0)) Hour)
             [ "ova poslijepodne"
             , "ovi popodne"
             ]
  , examples (datetime (2013, 2, 12, 13, 30, 0) Minute)
             [ "u 13:30"
             , "13:30"
             ]
  , examples (datetime (2013, 2, 12, 4, 45, 0) Second)
             [ "za 15 minuta"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 13, 0, 0), (2013, 2, 12, 17, 0, 0)) Hour)
             [ "poslije rucka"
             ]
  , examples (datetime (2013, 2, 12, 10, 30, 0) Minute)
             [ "10:30"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 0, 0), (2013, 2, 12, 12, 0, 0)) Hour)
             [ "ove jutro"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "sljedeci ponedjeljak"
             ]
  , examples (datetime (2013, 2, 12, 12, 0, 0) Hour)
             [ "u 12"
             , "u podne"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Hour)
             [ "u 12 u noci"
             , "u ponoc"
             ]
  , examples (datetime (2013, 3, 0, 0, 0, 0) Month)
             [ "ozujak"
             , "u ozujku"
             ]
  ]
