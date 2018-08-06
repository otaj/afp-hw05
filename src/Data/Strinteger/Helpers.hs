module Data.Strinteger.Helpers where

import           Data.List       (intercalate)
import           Data.List.Split (splitOn)
import           Data.Map        as M
import           Prelude         as P

zero = "zero" -- zero is very special value

units :: [(Integer, String)]
units = [ ( 0, zero)
        , ( 1, "one")
        , ( 2, "two")
        , ( 3, "three")
        , ( 4, "four")
        , ( 5, "five")
        , ( 6, "six")
        , ( 7, "seven")
        , ( 8, "eight")
        , ( 9, "nine")
        , (10, "ten")
        , (11, "eleven") -- if bored, watch: https://www.youtube.com/watch?v=NMS2VnDveP8
        , (12, "twelve")
        , (13, "thirteen")
        , (14, "fourteen")
        , (15, "fifteen")
        , (16, "sixteen")
        , (17, "seventeen")
        , (18, "eighteen")
        , (19, "nineteen")
        ]

tens :: [(Integer, String)]
tens = [ (1, "ten")
       , (2, "twenty")
       , (3, "thirty")
       , (4, "forty")
       , (5, "fifty")
       , (6, "sixty")
       , (7, "seventy")
       , (8, "eighty")
       , (9, "ninety")
       ]

scales :: [(Integer, String)]
scales = [ (  2, "hundred")
         , (  3, "thousand")
         , (  6, "million")
         , (  9, "billion")
         , ( 12, "trillion")
         , ( 15, "quadrillion")
         , ( 18, "quintillion")
         , ( 21, "sextillion")
         , ( 24, "septillion")
         , ( 27, "octillion")
         , ( 30, "nonillion")
         , ( 33, "decillion")
         , ( 36, "undecillion")
         , ( 39, "duodecillion")
         , ( 42, "tredecillion")
         , ( 45, "quattuordecillion")
         , ( 48, "quindecillion")
         , ( 51, "sexdecillion")
         , ( 54, "septendecillion")
         , ( 57, "octodecillion")
         , ( 60, "novemdecillion")
         , ( 63, "vigintillion")
         ]

highestPossible = 10^64 - 1

-- | Map where key is numword part and value is the tuple (scale, value)
word2numMap :: M.Map String (Integer, Integer)
word2numMap = unions [numunits, numtens, numscales]
             where
               numunits  = M.fromList [(str, ( 1, value)) | (value, str) <- units]
               numtens   = M.fromList [(str, (10, value)) | (value, str) <- tens]
               numscales = M.fromList [(str, (10^scale, 0)) | (scale, str) <- scales]

-- | Translate word to (scale, value) tuple if defined
word2num :: String -> Maybe (Integer, Integer)
word2num k = M.lookup k word2numMap

int2numParts :: Integer -> Integer -> [String]
int2numParts 0 0 = [zero]
int2numParts 0 _ = []
int2numParts num inside = getWordsTo1000 scale val ++ int2numParts (num - val * scale) (inside + 1)
   where (fullscale, rest) = divMod (floor (logBase 10 (fromInteger num))) 3
         scale = 10 ^ (fullscale * 3)
         val = num `quot` scale

getWordsTo1000 :: Integer -> Integer -> [String]
getWordsTo1000 scale val | scale > 1 = getWordsTo1000 0 val ++ case num2word scale 0 of
                                                                Just s -> [s]
                                                                Nothing -> []
                         | val >= 100 = hundList ++ getWordsTo1000 scale (val - n * 100)
                         | val >= 20 = tenList
                         | otherwise = unitList where
                            n = val `quot` 100
                            ntens = val `quot` 10
                            tenList = case (num2word 10 ntens, num2word 1 (val - ntens * 10)) of
                                (Just s1, Just "zero") -> [s1]
                                (Just s1, Just s2) -> [s1 ++ [separatorTens] ++ s2]
                                (_, _) -> []
                            unitList = case (num2word 1 val) of
                                (Just "zero") -> []
                                (Just s1)     -> [s1]
                                (_)           -> []
                            hundList = case (num2word 1 n, num2word 100 0) of
                                (Just s1, Just s2) -> [s1, s2]
                                (_, _)             -> []


numParts2Integer :: [String] -> Integer -> Integer -> Maybe Integer
numParts2Integer [] acc _ = Just acc
numParts2Integer (x:xs) acc inside | x == negativePrefix = numParts2Integer xs acc (inside + 1) >>= (\x -> Just (- x))
                                | separatorTens `elem` x = numParts2Integer ((splitOn [separatorTens] x) ++ xs) acc (inside + 1)
                                | otherwise = case (word2num x, acc) of
                                                         (Just (1, 0), _) -> if (P.null xs && inside == 0) then (Just 0) else Nothing
                                                         (Just (_, 0), 0) -> Nothing
                                                         (Just (100, 0), _) -> numParts2Integer xs (100 * acc) (inside + 1)
                                                         (Just (scale, 0), _) -> numParts2Integer xs 0 (inside + 1) >>= (\x -> Just (acc * scale + x))
                                                         (Just (scale, val), _) -> numParts2Integer xs ((scale * val) + acc) (inside + 1)
                                                         (Nothing, _) -> Nothing

-- | Map where key is (scale, value) tuple and numword is the value (inverse of word2numMap)
num2wordMap :: M.Map (Integer, Integer) String
num2wordMap = unions [wordunits, wordtens, wordscales]
             where
               wordunits  = M.fromList [(( 1, value), str) | (value, str) <- units]
               wordtens   = M.fromList [((10, value), str) | (value, str) <- tens]
               wordscales = M.fromList [((10^scale, 0), str) | (scale, str) <- scales]

-- | Translate scale and value to word if defined
num2word :: Integer -> Integer -> Maybe String
num2word scale value = M.lookup (scale, value) num2wordMap

separator = ' ' -- used between separate numerals
separatorTens = '-' -- used to connect tens and units: sixty-six
negativePrefix = "minus"
messageBadNumeral numeral = "Illegal English numeral: " ++ "'" ++ numeral ++ "'"
messageBadInteger integer = "Uncovertable Integer: " ++ show integer
