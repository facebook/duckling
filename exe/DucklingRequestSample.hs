{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -fno-full-laziness #-}
module Main (main) where
import Duckling.Debug
import Control.Monad
import System.Environment
import Duckling.Lang
import Data.Some
import Duckling.Dimensions.Types

main :: IO ()
main = do
  (repeatCount :: Int) <- read . head <$> getArgs
  void $ replicateM repeatCount $ void $ do
    debug EN "My number is 123" [This PhoneNumber,This Distance,This Numeral,This Email]
    debug EN "Wednesday 5:00PM 3/29/2017" [This Numeral,This Time]
    debug ZH "12:30pm" [This Time]
    debug EN "tomorrow at 4pm" [This Time]
    debug EN "Tomorrow at 12.30?" [This Time]
    debug EN "Wednesday 9am" [This Time]
    debug EN "Sure do! Will 11:30 work?" [This Time,This AmountOfMoney]
    debug EN "8:00am" [This Time]
