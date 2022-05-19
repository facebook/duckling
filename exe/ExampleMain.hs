-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.Applicative hiding (empty)
import Control.Arrow ((***))
import Control.Exception (SomeException, catch)
import Control.Monad (unless, when)
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString (ByteString, empty)
import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.String
import Data.Text (Text)
import Data.Time.LocalTime.TimeZone.Series
import Prelude
import System.Directory
import System.Environment (lookupEnv)
import TextShow
import Text.Read (readMaybe)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Snap.Core
import Snap.Http.Server

import Duckling.Core
import Duckling.Data.TimeZone
import Duckling.Dimensions (allDimensions)
import Duckling.Resolve (DucklingTime)

createIfMissing :: FilePath -> IO ()
createIfMissing f = do
  exists <- doesFileExist f
  unless exists $ writeFile f ""

shouldLog :: Maybe ConfigLog -> Bool
shouldLog Nothing = False
shouldLog (Just ConfigNoLog) = False
shouldLog _ = True

setupLogs :: Config a b -> IO ()
setupLogs conf = do
  let shouldLogErrors = shouldLog $ getErrorLog conf
  let shouldLogAccesses = shouldLog $ getAccessLog conf

  when (shouldLogErrors || shouldLogAccesses) $ createDirectoryIfMissing False "log"
  when shouldLogErrors $ createIfMissing "log/error.log"
  when shouldLogAccesses $ createIfMissing "log/access.log"

loadTZs :: IO (HashMap Text TimeZoneSeries)
loadTZs = do
  let defaultPath = "/usr/share/zoneinfo/"
  let fallbackPath = "/etc/zoneinfo/"
  loadTimeZoneSeries defaultPath `catch` (\(_ :: SomeException) -> loadTimeZoneSeries fallbackPath)

main :: IO ()
main = do
  tzs <- loadTZs
  p <- lookupEnv "PORT"
  conf <- commandLineConfig $
    maybe defaultConfig (`setPort` defaultConfig) (readMaybe =<< p)
  setupLogs conf
  httpServe conf $
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
    HashMap.fromList $ map dimText $ HashMap.toList supportedDimensions
  where
    dimText :: (Lang, [Seal Dimension]) -> (Text, [Text])
    dimText = (Text.toLower . showt) *** map (\(Seal d) -> toName d)


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
  latent <- getPostParam "latent"

  case t of
    Nothing -> do
      modifyResponse $ setResponseStatus 422 "Bad Input"
      writeBS "Need a 'text' parameter to parse"
    Just tx -> do
      let timezone = parseTimeZone tz
      now <- liftIO $ currentReftime tzs timezone

      let
        lang = parseLang l

        context = Context
          { referenceTime = maybe now (parseRefTime timezone) ref
          , locale = maybe (makeLocale lang Nothing) parseLocale loc
          }
        options = Options {withLatent = parseLatent latent}

        cleanupDims =
            LBS8.filter (/= '\\') -- strip out escape chars people throw in
          . stripSuffix "\"" -- remove trailing double quote
          . stripPrefix "\"" -- remote leading double quote

          where
            stripSuffix suffix str = fromMaybe str $ LBS.stripSuffix suffix str
            stripPrefix prefix str = fromMaybe str $ LBS.stripPrefix prefix str

        dims = fromMaybe (allDimensions lang) $ do
          queryDims <- fmap (cleanupDims . LBS.fromStrict) ds
          txtDims <- decode @[Text] queryDims
          pure $ mapMaybe parseDimension txtDims

        parsedResult = parse (Text.decodeUtf8 tx) context options dims

      writeLBS $ encode parsedResult
  where
    defaultLang = EN
    defaultLocale = makeLocale defaultLang Nothing
    defaultTimeZone = "America/Los_Angeles"
    defaultLatent = False

    parseDimension :: Text -> Maybe (Seal Dimension)
    parseDimension x = fromName x <|> fromCustomName x
      where
        fromCustomName :: Text -> Maybe (Seal Dimension)
        fromCustomName name = HashMap.lookup name m
        m = HashMap.fromList
          [ -- ("my-dimension", Seal (CustomDimension MyDimension))
          ]

    parseTimeZone :: Maybe ByteString -> Text
    parseTimeZone = maybe defaultTimeZone Text.decodeUtf8

    parseLocale :: ByteString -> Locale
    parseLocale x = maybe defaultLocale (`makeLocale` mregion) mlang
      where
        (mlang, mregion) = case chunks of
          [a, b] -> (readMaybe a :: Maybe Lang, readMaybe b :: Maybe Region)
          _      -> (Nothing, Nothing)
        chunks = map Text.unpack
          $ Text.split (== '_') $ Text.toUpper $ Text.decodeUtf8 x

    parseLang :: Maybe ByteString -> Lang
    parseLang l = fromMaybe defaultLang $ l >>=
      readMaybe . Text.unpack . Text.toUpper . Text.decodeUtf8

    parseRefTime :: Text -> ByteString -> DucklingTime
    parseRefTime timezone refTime = makeReftime tzs timezone utcTime
      where
        milliseconds = readMaybe $ Text.unpack $ Text.decodeUtf8 refTime
        utcTime = case milliseconds of
          Just msec -> posixSecondsToUTCTime $ fromInteger msec / 1000
          Nothing -> error "Please use milliseconds since epoch for reftime"

    parseLatent :: Maybe ByteString -> Bool
    parseLatent x = fromMaybe defaultLatent
      (readMaybe (Text.unpack $ Text.toTitle $ Text.decodeUtf8 $ fromMaybe empty x)::Maybe Bool)
