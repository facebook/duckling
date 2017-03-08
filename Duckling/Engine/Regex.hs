-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


module Duckling.Engine.Regex
  ( match
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Prelude
import qualified Text.Regex.Base as R
import qualified Text.Regex.PCRE as PCRE

match :: PCRE.Regex -> Text -> [[Text]]
match regex s = map (map Text.decodeUtf8) bss
  where
    bss :: [[ByteString]]
    bss = R.match regex $ Text.encodeUtf8 s
