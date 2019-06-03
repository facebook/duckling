-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


module Duckling.Email.Tests
  ( tests
  ) where

import Data.String
import Prelude
import Test.Tasty

import Duckling.Dimensions.Types
import Duckling.Email.Corpus
import Duckling.Testing.Asserts
import qualified Duckling.Email.DE.Tests as DE
import qualified Duckling.Email.EN.Tests as EN
import qualified Duckling.Email.FR.Tests as FR
import qualified Duckling.Email.IS.Tests as IS
import qualified Duckling.Email.IT.Tests as IT

tests :: TestTree
tests = testGroup "Email Tests"
  [ makeCorpusTest [This Email] corpus
  , makeNegativeCorpusTest [This Email] negativeCorpus
  , DE.tests
  , EN.tests
  , FR.tests
  , IS.tests
  , IT.tests
  ]
