module Duckling.Numeral.DE.NumParser (parseNumeral) where

import           Control.Applicative
import           Data.Char
import           Data.List

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

char :: Char -> Parser Char
char c = Parser p
  where
    p [] = Nothing
    p (x:xs)
      | x == c    = Just (x, xs)
      | otherwise = Nothing

inParser f = Parser . f . runParser

first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

instance Functor Parser where
  fmap = inParser . fmap . fmap . first

instance Applicative Parser where
  pure a = Parser (\s -> Just (a, s))
  (Parser fp) <*> xp = Parser $ \s ->
    case fp s of
      Nothing     -> Nothing
      Just (f,s') -> runParser (f <$> xp) s'

instance Alternative Parser where
  empty = Parser (const Nothing)
  Parser p1 <|> Parser p2 = Parser $ liftA2 (<|>) p1 p2

type NumParser = Parser Integer

(.+.) :: NumParser -> NumParser -> NumParser
(.+.) = (<*>) . ((+) <$>)

(.*.) :: NumParser -> NumParser -> NumParser
(.*.) = (<*>) . ((*) <$>)

(?) :: NumParser -> NumParser
(?) = (<|> Parser p) where p s = Just (0, s)

data NumItem = NumItem { base    :: NumParser
                       , plus10  :: NumParser
                       , times10 :: [NumParser]
                       }

defaultNumItem :: Integer -> String -> NumItem
defaultNumItem value form = NumItem { base    = p
                                    , plus10  = p .+. ten
                                    , times10 = p .*. ty:[]
                                    } where p = assign value form

type Assignment = Integer -> String -> NumParser

assign :: Assignment
assign = foldr ((<*>) . ((*) <$>) . (const 1 <$>) . char) . pure

ten :: NumParser
ten = assign 10 "zehn"

ty :: NumParser
ty = assign 10 "zig"

hundred :: NumParser
hundred = assign 100 "hundert"

thousand :: NumParser
thousand = assign 1000 "tausend"

und :: NumParser
und = (assign 0 "und")

one :: NumItem
one = (defaultNumItem 1 "ein") { plus10  = assign 11 "elf"
                               , times10 = [ ten ] }

two :: NumItem
two = (defaultNumItem 2 "zwei") { plus10  = assign 12 "zwölf"
                                , times10 = [ assign 20 "zwanzig" ] }
three :: NumItem
three = (defaultNumItem 3 "drei") { times10 = [ assign 30 "dreißig"
                                              , assign 30 "dreissig" ] }

four :: NumItem
four = defaultNumItem 4 "vier"

five :: NumItem
five = defaultNumItem 5 "fünf"

six :: NumItem
six = (defaultNumItem 6 "sechs") { plus10  = assign 16 "sechzehn"
                                 , times10 = [ assign 60 "sechzig" ] }

seven :: NumItem
seven = (defaultNumItem 7 "sieben") { plus10  = assign 17 "siebzehn"
                                    , times10 = [ assign 70 "siebzig" ] }

eight :: NumItem
eight = defaultNumItem 8 "acht"

nine :: NumItem
nine = defaultNumItem 9 "neun"

digitLexicon :: [NumItem]
digitLexicon = [one, two, three, four, five, six, seven, eight, nine]

from1to9 :: NumParser
from1to9 = foldr ((<|>) . base) empty digitLexicon

tensFrom20 :: NumParser
tensFrom20 = foldr (<|>) empty (foldr ((++) . times10) [] (tail digitLexicon))

from1to99 :: NumParser
from1to99 = (?) (from1to9 .+. und) .+. tensFrom20
            <|> foldr ((<|>) . plus10) empty digitLexicon
            <|> ten
            <|> from1to9

from1to999 :: NumParser
from1to999 = (?) (from1to9 .*. hundred .+. (?) und) .+. (?) from1to99

from1to999999 :: NumParser
from1to999999 = (?) (from1to999 .*. thousand .+. (?) und) .+. (?) from1to999

from1to999999' :: NumParser
from1to999999' = Parser p
  where p s
          | isPrefixOf "hundert" s || isPrefixOf "tausend" s
            = runParser from1to999999 ("ein" ++ s)
          | otherwise
            = runParser from1to999999 s

fromYear1100to1999 :: NumParser
fromYear1100to1999 = foldr ((<|>) . (.*. hundred) . plus10) empty digitLexicon
                     .+. (?) ((?) und .+. from1to99)

allNumerals :: NumParser
allNumerals = fromYear1100to1999
               <|> from1to999999'

removeInflection :: Maybe (Integer, String) -> Maybe Integer
removeInflection (Just (n, suffix))
  | n `mod` 10 == 1 && suffix `elem` inflection = Just n
  where inflection = ["s", "e", "em", "en", "er", "es"]
removeInflection (Just (n, "")) = Just n
removeInflection _ = Nothing

parseNumeral :: String -> Maybe Integer
parseNumeral = removeInflection . (runParser allNumerals)
