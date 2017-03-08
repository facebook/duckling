-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Url.Corpus
  ( corpus
  , negativeCorpus
  ) where

import Data.String
import Prelude

import Duckling.Testing.Types
import Duckling.Url.Types

corpus :: Corpus
corpus = (testContext, allExamples)

negativeCorpus :: NegativeCorpus
negativeCorpus = (testContext, examples)
  where
    examples =
      [ "foo"
      , "MYHOST"
      , "hey:42"
      , "25"
      ]

allExamples :: [Example]
allExamples = concat
  [ examples (UrlValue "http://www.bla.com")
             [ "http://www.bla.com"
             ]
  , examples (UrlValue "www.bla.com:8080/path")
             [ "www.bla.com:8080/path"
             ]
  , examples (UrlValue "https://myserver?foo=bar")
             [ "https://myserver?foo=bar"
             ]
  , examples (UrlValue "cnn.com/info")
             [ "cnn.com/info"
             ]
  , examples (UrlValue "bla.com/path/path?ext=%23&foo=bla")
             [ "bla.com/path/path?ext=%23&foo=bla"
             ]
  , examples (UrlValue "localhost")
             [ "localhost"
             ]
  , examples (UrlValue "localhost:8000")
             [ "localhost:8000"
             ]
  , examples (UrlValue "http://kimchi")
             [ "http://kimchi"
             ]
  , examples (UrlValue "https://500px.com:443/about")
             [ "https://500px.com:443/about"
             ]
  , examples (UrlValue "www2.foo-bar.net?foo=bar")
             [ "www2.foo-bar.net?foo=bar"
             ]
  ]
