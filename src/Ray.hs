module Ray(Ray(..), normalize, vector) where

import Vector (Vector, add, multiply)
import qualified Vector as V (normalize)

data Ray = Ray { origin :: Vector, direction :: Vector }

normalize :: Ray -> Ray
normalize (Ray origin direction) = Ray origin (V.normalize direction)

vector :: Ray -> Float -> Vector
vector (Ray origin direction) t = add origin (multiply direction t)
