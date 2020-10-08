-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -fno-full-laziness #-}

module Main (main) where

import Control.Monad
import System.Environment

import Duckling.Debug
import Duckling.Dimensions.Types
import Duckling.Locale

main :: IO ()
main = do
  (repeatCount :: Int) <- read . head <$> getArgs
  void $ replicateM repeatCount $ void $ do
    debug en "My number is 123" [Seal PhoneNumber,Seal Distance,Seal Numeral,Seal Email]
    debug en "Wednesday 5:00PM 3/29/2017" [Seal Numeral,Seal Time]
    debug zh "12:30pm" [Seal Time]
    debug en "tomorrow at 4pm" [Seal Time]
    debug en "Tomorrow at 12.30?" [Seal Time]
    debug en "Wednesday 9am" [Seal Time]
    debug en "Sure do! Will 11:30 work?" [Seal Time,Seal AmountOfMoney]
    debug en "8:00am" [Seal Time]
    where
      en = makeLocale EN Nothing
      zh = makeLocale ZH Nothing
