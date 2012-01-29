module Plane(Plane(..), fromPoints) where

import Prelude hiding (subtract)

import Data.Function (on)

import Vector(Vector(..), subtract, dot, cross, normalize)
import Ray(Ray(..), vector)
import Shape(Shape(..))

data Plane = Plane { planeNormal :: Vector, distance :: Float }

instance Shape Plane where
  intersections (Plane normal distance) ray@(Ray origin direction)
    | vD == 0 = []
    | otherwise = let t = (distance - (dot normal origin)) / vD in
        if t < 0 then [] else [vector ray t]
    where vD = dot normal direction
  
  normal (Plane normal _) _ = normal

  mapping (Plane normal distance) vector = undefined

fromPoints :: Vector -> Vector -> Vector -> Plane
fromPoints a@(Vector x y z) b c = Plane n $ dot n a
  where vA = subtract b a
        vB = subtract c a
        n = normalize $ cross vA vB
