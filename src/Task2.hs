{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2 where

import Parser
import ParserCombinators
import Control.Applicative (optional, empty, some, (<|>))
import Data.Char (isDigit)

-- | Date representation
--
-- Date parts are expected to be in following ranges
--
-- 'Day' in @[1..31]@
-- 'Month' in @[1..12]@
-- 'Year' is any non-negative integer
--
data Date = Date Day Month Year
  deriving (Show, Eq)

newtype Day   = Day   Int deriving (Show, Eq)
newtype Month = Month Int deriving (Show, Eq)
newtype Year  = Year  Int deriving (Show, Eq)

-- | Parses date in one of three formats given as BNF
--
-- @
-- date ::= dotFormat | hyphenFormat | usFormat
--
-- dotFormat ::= day "." month "." year
-- hyphenFormat ::= day "-" month "-" year
-- usFormat ::= monthName " " usDay " " year
--
-- usDay ::= nonZeroDigit | "1" digit | "2" digit | "30" | "31"
-- day ::= "0" nonZeroDigit | "1" digit | "2" digit | "30" | "31"
-- month ::= "0" nonZeroDigit | "10" | "11" | "12"
-- year ::= number
--
-- number ::= digit | number digit
-- digit ::= "0" | nonZeroDigit
-- nonZeroDigit ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
--
-- monthName ::= "Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" | "Jul" | "Aug" | "Sep" | "Oct" | "Nov" | "Dec"
-- @
--
-- Usage example:
--
-- >>> parse date "01.01.2012"
-- Parsed (Date (Day 1) (Month 1) (Year 2012)) (Input 10 "")
-- >>> parse date "12.12.2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 10 "")
-- >>> parse date "12-12-2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 10 "")
-- >>> parse date "Dec 12 2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 11 "")
-- >>> parse date "Jan 1 2012"
-- Parsed (Date (Day 1) (Month 1) (Year 2012)) (Input 10 "")
-- >>> parse date "Feb 31 2012"
-- Parsed (Date (Day 31) (Month 2) (Year 2012)) (Input 11 "")
-- >>> parse date "12/12/2012"
-- Failed [PosError 2 (Unexpected '/'),PosError 0 (Unexpected '1')]
--
date :: Parser Date
date = choice [dotFormat, hyphenFormat, usFormat]

nonZeroDigit :: Parser Char
nonZeroDigit = satisfy (\c -> c >= '1' && c <= '9')

digit :: Parser Char
digit = satisfy isDigit

number :: Parser Int
number = read <$> some digit

day :: Parser Int
day = choice [
    string "0" *> (read . pure <$> nonZeroDigit),
    string "1" *> (read . ("1"++) . pure <$> digit),
    string "2" *> (read . ("2"++) . pure <$> digit),
    string "30" *> pure 30,
    string "31" *> pure 31
  ]

usDay :: Parser Int
usDay = do
  firstDigit <- nonZeroDigit
  
  mbSecondDigit <- optional digit
  
  let numStr = case mbSecondDigit of
        Nothing -> [firstDigit]
        Just d -> [firstDigit, d]
      
      dayNum = read numStr

  if dayNum >= 1 && dayNum <= 31
    then return dayNum
    else empty


month :: Parser Int
month = choice [
    string "0" *> (read . pure <$> nonZeroDigit),
    string "10" *> pure 10,
    string "11" *> pure 11,
    string "12" *> pure 12
  ]

monthName :: Parser Int
monthName = choice [
    string "Jan" *> pure 1,
    string "Feb" *> pure 2,
    string "Mar" *> pure 3,
    string "Apr" *> pure 4,
    string "May" *> pure 5,
    string "Jun" *> pure 6,
    string "Jul" *> pure 7,
    string "Aug" *> pure 8,
    string "Sep" *> pure 9,
    string "Oct" *> pure 10,
    string "Nov" *> pure 11,
    string "Dec" *> pure 12
  ]

year :: Parser Int
year = read <$> (string "0" <|> some digit)

dotFormat :: Parser Date
dotFormat = Date <$> (Day <$> day) <* char '.' 
                  <*> (Month <$> month) <* char '.' 
                  <*> (Year <$> year)

hyphenFormat :: Parser Date
hyphenFormat = Date <$> (Day <$> day) <* char '-' 
                     <*> (Month <$> month) <* char '-' 
                     <*> (Year <$> year)

usFormat :: Parser Date
usFormat = 
  (\m d y -> Date d (Month m) y)
    <$> monthName <* char ' '
    <*> (Day <$> usDay) <* char ' '
    <*> (Year <$> year)