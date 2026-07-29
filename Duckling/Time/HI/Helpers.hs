{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.HI.Helpers where

import Data.Text (Text)

dayOfWeekMap :: [(Text, Int)]
dayOfWeekMap =
  [ ("सोमवार", 1)
  , ("मंगलवार", 2)
  , ("बुधवार", 3)
  , ("गुरुवार", 4)
  , ("शुक्रवार", 5)
  , ("शनिवार", 6)
  , ("रविवार", 7)
  ]


