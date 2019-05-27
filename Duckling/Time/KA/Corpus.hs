-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.KA.Corpus
  ( corpus
  , defaultCorpus
  , latentCorpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types hiding (examples)
import Duckling.Time.Corpus
import Duckling.Time.Types hiding (Month)
import Duckling.TimeGrain.Types
import Duckling.Resolve

corpus :: Corpus
corpus = (testContext {locale = makeLocale KA Nothing},
  testOptions, allExamples)

defaultCorpus :: Corpus
defaultCorpus = (testContext {locale = makeLocale KA Nothing},
  testOptions, allExamples)

latentCorpus :: Corpus
latentCorpus = (testContext {locale = makeLocale KA Nothing},
  testOptions {withLatent = True}, xs)
  where
    xs = concat
      [ examples (datetime (2013, 2, 24, 0, 0, 0) Day)
                 [ "24 თებერვალს"
                 ]
      , examples (datetime (2013, 2, 12, 7, 0, 0) Hour)
                 [ "7 საათზე"
                 ]
      , examples (datetime (2013, 5, 1, 0, 0, 0) Month)
                 [ "მაისი"
                 ]
      ]

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 12, 4, 30, 0) Second)
             [ "ახლა"
             , "ახლავე"
             , "ეხლა"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "დღეს"
             ]
  , examples (datetime (2013, 2, 1, 0, 0, 0) Day)
             [ "2/2013"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Year)
             [ "2014-ში"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "გუშინ"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "ხვალ"
             , "ხვალე"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "ორშაბათი"
             , "ამ ორშაბათს"
             , "ეს ორშაბათი"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "სამშაბათი"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "ხუთშაბათი"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "პარასკევი"
             ]
  , examples (datetime (2013, 2, 16, 0, 0, 0) Day)
             [ "შაბათი"
             ]
  , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
             [ "კვირა"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "1-ლი მარტი"
             , "პირველი მარტი"
             ]
  , examples (datetime (2013, 3, 3, 0, 0, 0) Day)
             [ "3 მარტი"
             ]
  , examples (datetime (2015, 3, 3, 0, 0, 0) Day)
             [ "3 მარტი 2015"
             , "2015 წლის მე-3 მარტი"
             , "2015-3-3"
             , "2015-03-03"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "15 თებერვალი"
             , "თებერვლის მეთხუთმეტე დღეს"
             , "თხუთმეტი თებერვალი"
             ]
  , examples (datetime (2013, 8, 8, 0, 0, 0) Day)
             [ "8 აგვისტო"
             ]
  , examples (datetime (2014, 10, 1, 0, 0, 0) Month)
             [ "2014 წლის ოქტომბერი"
             ]
  , examples (datetime (2015, 4, 14, 0, 0, 0) Day)
             [ "14 აპრილი 2015"
             , "2015 წლის 14 აპრილი"
             , "2015 წლის 14 აპრილს"
             ]
  , examples (datetime (2013, 2, 26, 0, 0, 0) Day)
             [ "შემდეგ სამშაბათს"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "შემდეგის შემდეგი პარასკევი"
             ]
  , examples (datetime (2014, 3, 1, 0, 0, 0) Month)
             [ "შემდეგი მარტი"
             ]
  , examples (datetime (2015, 3, 1, 0, 0, 0) Month)
             [ "შემდეგის შემდეგი მარტი"
             ]
  , examples (datetimeInterval ((2013, 1, 1, 0, 0, 0), (2013, 2, 1, 0, 0, 0)) Month)
             [ "წინა თვე"
             ]
  , examples (datetimeInterval ((2013, 3, 1, 0, 0, 0), (2013, 4, 1, 0, 0, 0)) Month)
             [ "შემდეგი თვე"
             ]
  , examples (datetimeInterval ((2013, 4, 1, 0, 0, 0), (2013, 7, 1, 0, 0, 0)) Quarter)
             [ "შემდეგ კვარტალში"
             ]
  , examples (datetimeInterval ((2012, 1, 1, 0, 0, 0), (2013, 1, 1, 0, 0, 0)) Year)
             [ "შარშან"
             , "წინა წელს"
             ]
  , examples (datetimeInterval ((2014, 1, 1, 0, 0, 0), (2015, 1, 1, 0, 0, 0)) Year)
             [ "მომავალი წელი"
             , "შემდეგ წელს"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "წინა კვირის კვირას"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "ბოლო სამშაბათს"
             ]
  , examples (datetime (2013, 2, 26, 0, 0, 0) Day)
             [ "შემდეგ სამშაბათს"
             ]
  , examples (datetime (2013, 2, 20, 0, 0, 0) Day)
             [ "შემდეგი ოთხშაბათი"
             ]
  , examples (datetime (2013, 2, 20, 0, 0, 0) Day)
             [ "შემდეგი კვირის ოთხშაბათი"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "შემდეგის შემდეგი პარასკევი"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "გუშინწინ"
             ]
  , examples (datetime (2013, 2, 10, 8, 0, 0) Hour)
             [ "გუშინწინ8-ზე"
             ]
  , examples (datetime (2013, 3, 25, 0, 0, 0) Day)
             [ "მარტის ბოლო ორშაბათი"
             ]
  , examples (datetime (2013, 10, 3, 0, 0, 0) Day)
             [ "ოქტომბრის მესამე დღე"
             , "ოქტომბრის მე-3 დღე"
             ]
  , examples (datetime (2013, 10, 1, 0, 0, 0) Day)
             [ "ოქტომბრის პირველი სამშაბათი"
             , "ოქტომბრის 1-ლი სამშაბათი"
             ]
  , examples (datetime (2013, 9, 17, 0, 0, 0) Day)
             [ "სექტემბრის მესამე სამშაბათი"
             , "სექტემბრის მე-3 სამშაბათი"
             ]
  , examples (datetime (2013, 10, 2, 0, 0, 0) Day)
             [ "ოქტომბრის პირველი ოთხშაბათი"
             , "ოქტომბრის 1-ლი ოთხშაბათი"
             ]
  , examples (datetime (2013, 10, 9, 0, 0, 0) Day)
             [ "ოქტომბრის მეორე ოთხშაბათი"
             , "ოქტომბრის მე-2 ოთხშაბათი"
             ]
  , examples (datetime (2013, 2, 13, 3, 18, 0) Minute)
             [ "3:18am"
             , "3:18a"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "15 საათზე"
             , "3PM"
             , "3pm"
             ]
  , examples (datetime (2013, 2, 12, 15, 15, 0) Minute)
             [ "4-ის 15 წუთზე"
             , "3 საათსა და 15 წუთზე"
             , "15:15"
             , "3:15pm"
             , "3:15PM"
             , "3:15p"
             ]
  , examples (datetime (2013, 2, 12, 15, 20, 0) Minute)
             [ "15 საათსა და 20 წუთზე"
             , "4-ის 20 წუთზე"
             , "3:20p"
             ]
  , examples (datetime (2013, 2, 12, 15, 30, 0) Minute)
             [ "4-ის ნახევარზე"
             , "15 საათსა და 30 წუთზე"
             , "15:30"
             , "3:30pm"
             , "3:30PM"
             , "330 p.m."
             , "3:30 p m"
             , "3:30"
             ]
  , examples (datetime (2013, 2, 12, 15, 23, 24) Second)
             [ "15:23:24"
             ]
  , examples (datetime (2013, 2, 12, 11, 45, 0) Minute)
             [ "11 საათსა და 45 წუთზე"
             , "11:45am"
             ]
  , examples (datetime (2013, 2, 12, 20, 0, 0) Hour)
             [ "20 საათი"
             , "20 საათზე"
             ]
  , examples (datetime (2013, 2, 16, 9, 0, 0) Hour)
             [ "შაბათს 9-ზე"
             ]
  , examples (datetime (2013, 2, 16, 9, 0, 0) Hour)
             [ "შაბათს 9 საათზე"
             ]
  , examples (datetime (2013, 2, 5, 4, 0, 0) Hour)
             [ "7 დღის წინ"
             , "შვიდი დღის წინ"
             ]
  , examples (datetime (2013, 1, 29, 4, 0, 0) Hour)
             [ "14 დღის წინ"
             , "თოთხმეტი დღის წინ"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "1 კვირის წინ"
             , "ერთი კვირის წინ"
             ]
  , examples (datetime (2013, 1, 22, 0, 0, 0) Day)
             [ "3 კვირის წინ"
             , "სამი კვირის წინ"
             ]
  , examples (datetime (2012, 11, 12, 0, 0, 0) Day)
             [ "სამი თვის წინ"
             , "3 თვის წინ"
             ]
  , examples (datetime (2011, 2, 1, 0, 0, 0) Month)
             [ "ორი წლის წინ"
             , "ორი წლის წინ"
             ]
  , examples (datetimeInterval ((2013, 6, 1, 0, 0, 0), (2013, 9, 1, 0, 0, 0)) Day)
             [ "ამ ზაფხულს"
             , "ამ ზაფხულში"
             ]
  , examples (datetimeInterval ((2012, 12, 1, 0, 0, 0), (2013, 3, 1, 0, 0, 0)) Day)
             [ "ამ ზამთარს"
             , "ამ ზამთარში"
             ]
  , examples (datetimeInterval ((2012, 9, 23, 0, 0, 0), (2012, 12, 20, 0, 0, 0)) Day)
             [ "წინა სეზონზე"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 18, 0, 0), (2013, 2, 12, 0, 0, 0)) Hour)
             [ "გუშინ ღამე"
             , "გუშინ ღამით"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 21, 0, 0), (2013, 2, 12, 0, 0, 0)) Hour)
             [ "გუშინ გვიან ღამე"
             , "გუშინ გვიან ღამით"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 18, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
             [ "დღეს საღამოს"
             , "დღეს ღამე"
             ]
  , examples (datetimeInterval ((2013, 2, 9, 0, 0, 0), (2013, 2, 11, 0, 0, 0)) Day)
             [ "გასულ უქმეებზე"
             , "გასულ შაბათ-კვირას"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 18, 0, 0), (2013, 2, 14, 0, 0, 0)) Hour)
             [ "ხვალ საღამოს"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 18, 0, 0), (2013, 2, 12, 0, 0, 0)) Hour)
             [ "გუშინ საღამოს"
             ]
  , examples (datetimeInterval ((2013, 2, 16, 0, 0, 0), (2013, 2, 18, 0, 0, 0)) Day)
             [ "ამ შაბათკვირას"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 4, 0, 0), (2013, 2, 18, 12, 0, 0)) Hour)
             [ "ორშაბათს დილას"
             , "ორშაბათს დილა"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 4, 0, 0), (2013, 2, 15, 12, 0, 0)) Hour)
             [ "15 თებერვალი დილა"
             , "15 თებერვალს დილას"
             , "15 თებერვლის დილა"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 29, 58), (2013, 2, 12, 4, 30, 1)) Second)
             [ "ბოლო 2 წამი"
             , "ბოლო 2 წამში"
             ]
  , examples (datetimeInterval ((2013, 7, 13, 0, 0, 0), (2013, 7, 16, 0, 0, 0)) Day)
             [ "ივლისი 13-15"
             , "13-15 ივლისი"
             ]
  , examples (datetimeInterval ((2013, 8, 8, 0, 0, 0), (2013, 8, 13, 0, 0, 0)) Day)
             [ "8-12 აგვისტო"
             ]
  , examples (datetimeInterval ((2013, 2, 14, 9, 30, 0), (2013, 2, 14, 11, 1, 0)) Minute)
             [ "ხუთშაბათი 9:30-დან 11:00-მდე"
             , "ხუთშაბათს 10-ის ნახევრიდან 11:00-მდე"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 2, 26, 0, 0, 0)) Second)
             [ "ორი კვირის განმავლობაში"
             , "2 კვირის განმავლობაში"
             ]
  , examples (datetimeInterval ((2013, 2, 21, 0, 0, 0), (2013, 3, 1, 0, 0, 0)) Day)
             [ "თვის ბოლო"
             , "თვის ბოლოსთვის"
             , "თვის ბოლოსკენ"
             , "ამ თვის ბოლო"
             , "ამ თვის ბოლოსთვის"
             , "ამ თვის ბოლოსკენ"
             ]
  , examples (datetime (2013, 2, 12, 14, 0, 0) Hour)
             [ "დღეს შუადღის 2-ზე"
             , "დღეს დღის 2-ზე"
             , "დღეს შუადღის ორზე"
             ]
  , examples (datetime (2013, 2, 13, 15, 0, 0) Hour)
             [ "ხვალ დღის 3-ზე"
             , "ხვალ დღის სამზე"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 18, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
             [ "საღამოს"
             , "დღეს საღამოს"
             , "დღეს ღამე"
             , "ღამე"
             , "ღამით"
             ]
  , examples (datetime (2013, 2, 12, 13, 30, 0) Minute)
             [ "1 საათსა და 30 წუთზე"
             , "2-ის ნახევარზე"
             , "2-ის ნახევარი"
             , "1 საათი და 30 წუთი"
             ]
  , examples (datetime (2013, 2, 12, 4, 45, 0) Second)
             [ "15 წუთში"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 0, 0), (2013, 2, 12, 12, 0, 0)) Hour)
             [ "დღეს დილას"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "ორშაბათი"
             , "ორშაბათს"
             ]
  , examples (datetime (2013, 2, 12, 12, 0, 0) Hour)
             [ "12 საათი"
             , "12 საათზე"
             , "დღის 12 საათზე"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Month)
             [ "მარტი"
             ]
  , examples (datetime (2013, 2, 13, 17, 0, 0) Hour)
             [ "ხვალ დღის 5"
             , "ხვალ შუადღის 5"
             , "ხვალ დღის 5-ზე"
             , "ხვალ შუადღის 5-ზე"
             , "ხვალ დღის 5 საათი"
             , "ხვალ შუადღის 5 საათი"
             , "ხვალ დღის 5 საათზე"
             , "ხვალ შუადღის 5 საათზე"
             ]
  , examples (datetime (2013, 2, 12, 10, 30, 0) Minute)
             [ "დილის 10:30"
             , "დილის 10 საათსა და 30 წუთზე"
             , "დილის 10 საათი და 30 წუთი"
             ]
  , examples (datetime (2013, 2, 12, 23, 0, 0) Hour)
             [ "დღეს ღამის 11"
             , "დღეს ღამის 11-ზე"
             , "დღეს ღამის 11 საათი"
             , "დღეს ღამის 11 საათზე"
             ]
  , examples (datetime (2013, 2, 12, 4, 23, 0) Minute)
             [ "4:23-ზე"
             ]
  , examples (datetimeInterval ((2013, 3, 1, 0, 0, 0), (2013, 3, 11, 0, 0, 0)) Day)
             [ "მარტის დასაწყისი"
             , "მარტის დასაწყისში"
             ]
  , examples (datetimeInterval ((2013, 3, 11, 0, 0, 0), (2013, 3, 21, 0, 0, 0)) Day)
             [ "მარტის შუა"
             ]
  , examples (datetimeInterval ((2013, 3, 21, 0, 0, 0), (2013, 4, 1, 0, 0, 0)) Day)
             [ "მარტის ბოლო"
             ]
  , examples (datetimeInterval ((2013, 10, 26, 0, 0, 0), (2013, 10, 28, 0, 0, 0)) Day)
             [ "ოქტომბრის ბოლო უქმეები"
             , "ოქტომბრის ბოლო შაბათ-კვირა"
             ]
  , examples (datetimeInterval ((2017, 12, 23, 0, 0, 0), (2017, 12, 25, 0, 0, 0)) Day)
             [ "2017 წლის დეკემბრის ბოლო უქმეები"
             ]
  , examples (datetimeInterval ((2013, 8, 27, 0, 0, 0), (2013, 8, 30, 0, 0, 0)) Day)
             [ "27-29 აგვისტო"
             ]
  , examples (datetimeInterval ((2013, 10, 23, 0, 0, 0), (2013, 10, 27, 0, 0, 0)) Day)
             [ "23-26 ოქტომბერი"
             ]
  , examples (datetimeInterval ((2013, 9, 1, 0, 0, 0), (2013, 9, 9, 0, 0, 0)) Day)
             [ "1-8 სექტემბერი"
             ]
  , examples (datetimeInterval ((2013, 9, 12, 0, 0, 0), (2013, 9, 17, 0, 0, 0)) Day)
             [ "12-16 სექტემბერი"
             ]
  , examples (datetimeInterval ((2013, 8, 19, 0, 0, 0), (2013, 8, 22, 0, 0, 0)) Day)
             [ "19-21 აგვისტო"
             ]
  , examples (datetimeInterval ((2013, 4, 21, 0, 0, 0), (2013, 5, 1, 0, 0, 0)) Day)
             [ "აპრილის ბოლო"
             ]
  , examples (datetimeInterval ((2014, 1, 1, 0, 0, 0), (2014, 1, 11, 0, 0, 0)) Day)
             [ "იანვრის დასაწყისი"
             ]
  , examples (datetimeInterval ((2013, 1, 1, 0, 0, 0), (2013, 4, 1, 0, 0, 0)) Month)
             [ "წლის დასაწყისი"
             , "ამ წლის დასაწყისი"
             ]
  , examples (datetimeInterval ((2012, 12, 1, 0, 0, 0), (2013, 2, 1, 0, 0, 0)) Month)
             [ "გასულ ორ თვეში"
             , "გასულ 2 თვეში"
             , "გასულ 2 თვეს"
             ]
  , examples (datetimeInterval ((2013, 1, 21, 0, 0, 0), (2013, 2, 11, 0, 0, 0)) Week)
             [ "გასული 3 კვირა"
             ]
  , examples (datetimeInterval ((2013, 5, 1, 0, 0, 0), (2013, 6, 1, 0, 0, 0)) Month)
             [ "მიმდინარე წლის მაისში"
             , "წელს მაისში"
             ]
  , examples (datetimeInterval ((2013, 4, 1, 0, 0, 0), (2013, 7, 1, 0, 0, 0)) Quarter)
             [ "წელს მეორე კვარტალში"
             ]
  ]
