module Sphere(Sphere(..)) where

import Vector (Vector, dot, length2)
import Ray (Ray(..), vector)
import Shape (Shape(..))

data Sphere = Sphere { center :: Vector, radius :: Float }

instance Shape Sphere where
  withRay (Sphere center radius) ray@(Ray origin direction)
    | det == 0 = map (vector ray) $ filter (>= 0) [cd - od]
    | det > 0 = let d = sqrt det in map (vector ray) $ filter (>= 0) [cd - od + d, cd - od - d]
    | otherwise = []
    where cd = dot center direction
          od = dot origin direction
          det = (od - cd)**2 - (length2 origin) - (length2 center) + 2*(dot center origin) + radius**2