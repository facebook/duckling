-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


import Data.String
import Test.Tasty

import Duckling.Tests
import Prelude

main :: IO ()
main = defaultMain tests
