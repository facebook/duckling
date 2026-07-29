-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}
module Duckling.Ranking.Classifiers.MR_XX (classifiers) where

import Data.String
import Prelude
import qualified Data.HashMap.Strict as HashMap
import Duckling.Ranking.Types

classifiers :: Classifiers
classifiers = HashMap.fromList []
