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
import Data.String
import Data.Text (Text)
import Data.Time.LocalTime.TimeZone.Series
import Prelude
import System.Directory
import TextShow
import Text.Read (readMaybe)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Snap.Core
import Snap.Http.Server

import Duckling.Core
import Duckling.Data.TimeZone
import Duckling.Resolve (DucklingTime)

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
  loc <- getPostParam "locale"
  ref <- getPostParam "reftime"

  case t of
    Nothing -> do
      modifyResponse $ setResponseStatus 422 "Bad Input"
      writeBS "Need a 'text' parameter to parse"
    Just tx -> do
      let timezone = parseTimeZone tz
      now <- liftIO $ currentReftime tzs timezone
      let
        context = Context
          { referenceTime = maybe now (parseRefTime timezone) ref
          , locale = maybe (makeLocale (parseLang l) Nothing) parseLocale loc
          }

        dimParse = fromMaybe [] $ decode $ LBS.fromStrict $ fromMaybe "" ds
        dims = mapMaybe fromName dimParse

        parsedResult = parse (Text.decodeUtf8 tx) context dims

      writeLBS $ encode parsedResult
  where
    defaultLang = EN
    defaultLocale = makeLocale defaultLang Nothing
    defaultTimeZone = "America/Los_Angeles"

    parseTimeZone :: Maybe ByteString -> Text
    parseTimeZone = fromMaybe defaultTimeZone . fmap Text.decodeUtf8

    parseLocale :: ByteString -> Locale
    parseLocale x = maybe defaultLocale (`makeLocale` mregion) mlang
      where
        (mlang, mregion) = case chunks of
          [a, b] -> (readMaybe a :: Maybe Lang, readMaybe b :: Maybe Region)
          _      -> (Nothing, Nothing)
        chunks = map Text.unpack . Text.split (== '_') . Text.toUpper
          $ Text.decodeUtf8 x

    parseLang :: Maybe ByteString -> Lang
    parseLang l = fromMaybe defaultLang $ l >>=
      readMaybe . Text.unpack . Text.toUpper . Text.decodeUtf8

    parseRefTime :: Text -> ByteString -> DucklingTime
    parseRefTime timezone refTime = makeReftime tzs timezone utcTime
      where
        msec = read $ Text.unpack $ Text.decodeUtf8 refTime
        utcTime = posixSecondsToUTCTime $ fromInteger msec / 1000
