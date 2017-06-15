-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Arrow ((***))
import Control.Monad (unless)
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.Text (Text)
import Data.Time.LocalTime.TimeZone.Series
import Data.String
import Prelude
import System.Directory
import TextShow
import Text.Read (readMaybe)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Snap.Core
import Snap.Http.Server

import Duckling.Core
import Duckling.Data.TimeZone

createIfMissing :: FilePath -> IO ()
createIfMissing f = do
  exists <- doesFileExist f
  unless exists $ writeFile f ""

setupLogs :: IO ()
setupLogs = do
  createDirectoryIfMissing False "log"
  createIfMissing "log/error.log"
  createIfMissing "log/access.log"

main :: IO ()
main = do
  setupLogs
  tzs <- loadTimeZoneSeries "/usr/share/zoneinfo/"
  quickHttpServe $
    ifTop (writeBS "quack!") <|>
    route
      [ ("targets", method GET targetsHandler)
      , ("parse", method POST $ parseHandler tzs)
      ]

-- | Return which languages have which dimensions
targetsHandler :: Snap ()
targetsHandler = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS $ encode $
    HashMap.fromList . map dimText $ HashMap.toList supportedDimensions
  where
    dimText :: (Lang, [Some Dimension]) -> (Text, [Text])
    dimText = (Text.toLower . showt) *** map (\(This d) -> toName d)


-- | Parse some text into the given dimensions
parseHandler :: HashMap Text TimeZoneSeries -> Snap ()
parseHandler tzs = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  t <- getPostParam "text"
  l <- getPostParam "lang"
  ds <- getPostParam "dims"
  tz <- getPostParam "tz"

  case t of
    Nothing -> do
      modifyResponse $ setResponseStatus 422 "Bad Input"
      writeBS "Need a 'text' parameter to parse"
    Just tx -> do
      refTime <- liftIO $ currentReftime tzs $
                   fromMaybe defaultTimeZone $ Text.decodeUtf8 <$> tz
      let
        context = Context
          { referenceTime = refTime
          , lang = parseLang l
          }

        dimParse = fromMaybe [] $ decode $ LBS.fromStrict $ fromMaybe "" ds
        dims = mapMaybe fromName dimParse

        parsedResult = parse (Text.decodeUtf8 tx) context dims

      writeLBS $ encode parsedResult
  where
    defaultLang = EN
    defaultTimeZone = "America/Los_Angeles"

    parseLang :: Maybe ByteString -> Lang
    parseLang l = fromMaybe defaultLang $ l >>=
      readMaybe . Text.unpack . Text.toUpper . Text.decodeUtf8
