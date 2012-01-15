module Triangle(Triangle(..)) where

import Prelude hiding (subtract)

import Vector (Vector(..), subtract, dot, cross, normalize)
import Ray (Ray(..))
import Shape (Shape(..))
import Plane (Plane(..), fromPoints)

data Triangle = Triangle { vertexA :: Vector, vertexB :: Vector, vertexC :: Vector }

instance Shape Triangle where
  withRay (Triangle a b c) ray@(Ray origin direction) = filter inTriangle hits
    where plane = fromPoints a b c
          hits = withRay plane ray
          inTriangle p = (u >= 0) && (v >= 0) && (u + v < 1)
            where v0 = subtract c a
                  v1 = subtract b a
                  v2 = subtract p a
                  dot00 = dot v0 v0 
                  dot01 = dot v0 v1 
                  dot02 = dot v0 v2 
                  dot11 = dot v1 v1 
                  dot12 = dot v1 v2
                  invDenom = 1 / (dot00 * dot11 - dot01 * dot01)
                  u = (dot11 * dot02 - dot01 * dot12) * invDenom
                  v = (dot00 * dot12 - dot01 * dot02) * invDenom
