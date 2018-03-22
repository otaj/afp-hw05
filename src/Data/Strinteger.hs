module Data.Strinteger where

import Data.Maybe
-- You might need to use intercalate and splitOn (similar to words/unwords)
import Data.List (intercalate)
import Data.List.Split (splitOn)

-- Use Data.Strinteger.Helpers submodule
-- In case of need, feel free to change or enhance Helpers or create own
-- submodule
--
-- DO NOT HARDCODE ANY STRINGs/CHARs IN THIS MODULE!
import qualified Data.Strinteger.Helpers as SH

-- | Strinteger type (wrapper) for English numerals
newtype Strinteger = Strinteger String
                   deriving (Show, Read)

instance Bounded Strinteger where
   maxBound = pack SH.highestPossible
   minBound = negate maxBound

   -- | Pack Integer into Strinteger (English numeral string)
pack :: Integer -> Strinteger
pack integer = Strinteger $ fromMaybe err (integer2EngNumeral integer)
               where
                 err = error $ SH.messageBadInteger integer

-- | Unpack Strinteger (English numeral string) to Integer
unpack :: Strinteger -> Integer
unpack (Strinteger numeral) = fromMaybe err (engNumeral2Integer numeral)
                              where
                                err = error $ SH.messageBadNumeral numeral


-- | Translate Integer to String (if possible)
-- TODO: implement Integer->String translation
integer2EngNumeral :: Integer -> Maybe String
integer2EngNumeral = undefined

-- | Translate String to Integer (if possible)
-- TODO: implement String->Integer translation
engNumeral2Integer :: String -> Maybe Integer
engNumeral2Integer = undefined

-- TODO: implement Strinteger instances of Num, Ord, Eq, Enum, Real, and Integral
instance Eq Strinteger where
    (==) = undefined

instance Ord Strinteger where
    compare = undefined

instance Num Strinteger where
    (+) = undefined
    (*) = undefined
    negate = undefined
    abs = undefined
    signum = undefined
    fromInteger = undefined

instance Enum Strinteger where
    toEnum = undefined
    fromEnum = undefined

instance Real Strinteger where
    toRational = undefined

instance Integral Strinteger where
    quotRem = undefined
    toInteger = undefined
