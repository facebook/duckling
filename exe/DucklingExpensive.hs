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
    debug EN "Monday 3rd at 9.30am or 2.30pm, Saturday 8th at 10.30am, Tuesday 11th at 2pm, Wednesday 12th at 2.30pm, Friday 14th at 12.30pm xx" [This Time]
    debug ES "Horario es de Lunes a Viernes de 2 pm a 10 pm. S\195\161bado de 9 am a 7 pm" [This Time]
