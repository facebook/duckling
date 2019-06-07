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
    debug en "Monday 3rd at 9.30am or 2.30pm, Saturday 8th at 10.30am, Tuesday 11th at 2pm, Wednesday 12th at 2.30pm, Friday 14th at 12.30pm xx" [This Time]
    debug es "Horario es de Lunes a Viernes de 2 pm a 10 pm. S\195\161bado de 9 am a 7 pm" [This Time]
    where
      en = makeLocale EN Nothing
      es = makeLocale ES Nothing
