-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -fno-full-laziness #-}

module Main (main) where

import Control.Monad
import Data.Some
import System.Environment

import Duckling.Debug
import Duckling.Dimensions.Types
import Duckling.Locale

main :: IO ()
main = do
  (repeatCount :: Int) <- read . head <$> getArgs
  void $ replicateM repeatCount $ void $ do
    debug en "My number is 123" [This PhoneNumber,This Distance,This Numeral,This Email]
    debug en "Wednesday 5:00PM 3/29/2017" [This Numeral,This Time]
    debug zh "12:30pm" [This Time]
    debug en "tomorrow at 4pm" [This Time]
    debug en "Tomorrow at 12.30?" [This Time]
    debug en "Wednesday 9am" [This Time]
    debug en "Sure do! Will 11:30 work?" [This Time,This AmountOfMoney]
    debug en "8:00am" [This Time]
    where
      en = makeLocale EN Nothing
      zh = makeLocale ZH Nothing
