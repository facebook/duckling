-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}

module Duckling.Regex.Types
  ( GroupMatch(..)
  ) where

import Control.DeepSeq
import Data.Hashable
import Data.Text (Text)
import GHC.Generics
import Prelude
import Duckling.Resolve (Resolve(..))

data GroupMatch = GroupMatch [Text]
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance Resolve GroupMatch where
  type ResolvedValue GroupMatch = ()
  resolve _ _ _ = Nothing
