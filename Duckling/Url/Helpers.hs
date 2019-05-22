-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.




module Duckling.Url.Helpers
  ( url
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

import qualified Duckling.Url.Types as TUrl
import Duckling.Url.Types (UrlData(..))

-- -----------------------------------------------------------------
-- Patterns

-- -----------------------------------------------------------------
-- Production

url :: Text -> Text -> UrlData
url value domain = UrlData
  {TUrl.value = value, TUrl.domain = Text.toLower domain}
