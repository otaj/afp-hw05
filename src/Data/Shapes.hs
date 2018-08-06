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

class (Validable a) => Shape2D a where
    area :: a -> Double
    area = undefined
    circumference :: a -> Double
    circumference = undefined

instance Validable Circle where
    valid (Circle x) = x > 0.0
instance Validable Triangle where
    valid (EquilateralTriangle x) = x > 0.0
    valid (IsoscelesTriangle a c) = 2 * c > a && c > 0.0 && a > 0.0
    valid (ScaleneTriangle a b c) = ((a + b) > c) && ((a + c) > b) && ((b + c) > a) && a > 0.0 && b > 0.0 && c > 0.0
instance Validable Quadrilateral where
    valid (Square x)      = x > 0.0
    valid (Rectangle a b) = a > 0.0 && b > 0.0

instance Shape2D Circle where
    area x          | not $ valid x = 0.0
    area (Circle r) = r * r * pi
    circumference x          | not $ valid x = 0.0
    circumference (Circle r) = 2 * pi * r

instance Shape2D Triangle where
    area x                                | not $ valid x = 0.0
    area (EquilateralTriangle x)          = ((sqrt 3) / 4) * x * x
    area x@(IsoscelesTriangle a c) = sqrt (p*(p-a)*(p-c)*(p-c)) where p = (circumference x) / 2
    area x@(ScaleneTriangle a b c)          = sqrt (p*(p-a)*(p-b)*(p-c)) where p = (circumference x) / 2
    circumference x                       | not $ valid x = 0.0
    circumference (EquilateralTriangle x) = 3 * x
    circumference (IsoscelesTriangle a c) = a + c * 2
    circumference (ScaleneTriangle a b c) = a + b + c

instance Shape2D Quadrilateral where
    area x               | not $ valid x = 0.0
    area (Square x)      = x * x
    area (Rectangle a b) = a * b
    circumference x               | not $ valid x = 0.0
    circumference (Square x)      = 4 * x
    circumference (Rectangle a b) = 2 * (a + b)
