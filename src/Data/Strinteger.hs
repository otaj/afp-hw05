module Data.Strinteger where

import           Data.List.Split         (splitOn)
import           Data.Maybe
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
integer2EngNumeral num | abs num > SH.highestPossible = Nothing
                       | num < 0 = (Just (unwords (SH.negativePrefix : SH.int2numParts (negate num) 0)))
                       | otherwise = (Just (unwords (SH.int2numParts num 0)))


-- | Translate String to Integer (if possible)
engNumeral2Integer :: String -> Maybe Integer
engNumeral2Integer numeral = SH.numParts2Integer (splitOn [SH.separator] numeral) 0 0

instance Eq Strinteger where
    (==) a b = (unpack a) == (unpack b)

instance Ord Strinteger where
    compare a b = compare (unpack a) (unpack b)

instance Num Strinteger where
    (+) a b = pack $ (unpack a) + (unpack b)
    (*) a b = pack $ (unpack a) * (unpack b)
    negate = pack . negate . unpack
    abs = pack . abs . unpack
    signum = pack . signum . unpack
    fromInteger = pack

instance Enum Strinteger where
    toEnum = pack . fromIntegral
    fromEnum = fromIntegral . unpack

instance Real Strinteger where
    toRational = undefined

instance Integral Strinteger where
    quotRem a b = (pack uA, pack uB) where (uA, uB) = quotRem (unpack a) (unpack b)
    toInteger = unpack
