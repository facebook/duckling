-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

import Prelude
import Data.String

import Duckling.Core
import Duckling.Data.TimeZone

main :: IO ()
main = do
  tzs <- loadTimeZoneSeries "/usr/share/zoneinfo/"
  refTime <- currentReftime tzs "America/Los_Angeles"
  let context = Context {referenceTime = refTime, lang = EN}
  print $ parse "tomorrow at 6ish pm" context [Some Time]
