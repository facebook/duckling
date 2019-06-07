-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


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
corpus = (testContext, testOptions, allExamples)

negativeCorpus :: NegativeCorpus
negativeCorpus = (testContext, testOptions, examples)
  where
    examples =
      [ "foo"
      , "MYHOST"
      , "hey:42"
      , "25"
      ]

allExamples :: [Example]
allExamples = concat
  [ examples (UrlData "http://www.bla.com" "bla.com")
             [ "http://www.bla.com"
             ]
  , examples (UrlData "www.bla.com:8080/path" "bla.com")
             [ "www.bla.com:8080/path"
             ]
  , examples (UrlData "https://myserver?foo=bar" "myserver")
             [ "https://myserver?foo=bar"
             ]
  , examples (UrlData "cnn.com/info" "cnn.com")
             [ "cnn.com/info"
             ]
  , examples (UrlData "bla.com/path/path?ext=%23&foo=bla" "bla.com")
             [ "bla.com/path/path?ext=%23&foo=bla"
             ]
  , examples (UrlData "localhost" "localhost")
             [ "localhost"
             ]
  , examples (UrlData "localhost:8000" "localhost")
             [ "localhost:8000"
             ]
  , examples (UrlData "http://kimchi" "kimchi")
             [ "http://kimchi"
             ]
  , examples (UrlData "https://500px.com:443/about" "500px.com")
             [ "https://500px.com:443/about"
             ]
  , examples (UrlData "www2.foo-bar.net?foo=bar" "foo-bar.net")
             [ "www2.foo-bar.net?foo=bar"
             ]
  , examples (UrlData "https://api.wit.ai/message?q=hi" "api.wit.ai")
             [ "https://api.wit.ai/message?q=hi"
             ]
  , examples (UrlData "aMaZon.co.uk/?page=home" "amazon.co.uk")
             [ "aMaZon.co.uk/?page=home"
             ]
  , examples (UrlData "https://en.wikipedia.org/wiki/Uniform_Resource_Identifier#Syntax" "en.wikipedia.org")
             [ "https://en.wikipedia.org/wiki/Uniform_Resource_Identifier#Syntax"
             ]
  , examples (UrlData "http://example.com/data.csv#cell=4,1-6,2" "example.com")
             [ "http://example.com/data.csv#cell=4,1-6,2"
             ]
  , examples (UrlData "http://example.com/bar.webm#t=40,80&xywh=160,120,320,240" "example.com")
             [ "http://example.com/bar.webm#t=40,80&xywh=160,120,320,240"
             ]
  ]
