module Data.Shapes where

--------------------------------------------------------------------------------
-- DO NOT CHANGE DATA TYPES DEFINITIONS

newtype Circle = Circle { ciRadius :: Double }
               deriving (Show, Read, Eq)

data Triangle = EquilateralTriangle { etSide :: Double }
              | IsoscelesTriangle { itBase :: Double, itLeg :: Double }
              | ScaleneTriangle { stSideA :: Double, stSideB :: Double, stSideC :: Double }
              deriving (Show, Read, Eq)

data Quadrilateral = Square { sqSide :: Double}
                   | Rectangle { reSideA :: Double, reSideB :: Double }
                   deriving (Show, Read, Eq)

--------------------------------------------------------------------------------

class Validable a where
  valid :: a -> Bool
  valid = undefined

-- TODO: complete instances for each type to check validity by `valid` function
instance Validable Circle
instance Validable Triangle
instance Validable Quadrilateral

-- TODO: create appropriate typeclass for 2D shapes (subclass of Validable)
-- TODO: write instances for the types to compute circumference and area

-- Note: this dummy functions should be placed in typeclass
area = undefined
circumference = undefined
