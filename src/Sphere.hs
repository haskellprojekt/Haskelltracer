module Sphere(Sphere(..)) where

import Prelude hiding (subtract)
import Data.List (sort)
import Data.Maybe (listToMaybe)

import Vector (Vector, subtract, dot, length2, normalize)
import Ray (Ray(..), vector)
import Shape (Hit(..), Shape(..))

data Sphere = Sphere { center :: Vector, radius :: Float }

instance Shape Sphere where
  intersection (Sphere center radius) ray@(Ray origin direction)
    | det == 0 = listToMaybe $ map (hit ray) $ filter (>= 0) [cd - od]
    | det > 0 = let d = sqrt det in listToMaybe $ map (hit ray) $ sort $ filter (>= 0) [cd - od + d, cd - od - d]
    | otherwise = Nothing
    where cd = dot center direction
          od = dot origin direction
          det = (od - cd)**2 - (length2 origin) - (length2 center) + 2*(dot center origin) + radius**2
          hit ray t = Hit t $ vector ray t
  
  normal (Sphere center _) vector = normalize $ subtract vector center

  mapping (Sphere center radius) vector = undefined
