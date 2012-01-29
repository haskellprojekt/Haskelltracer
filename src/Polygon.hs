module Polygon(Polygon(..)) where

data Polygon = Polygon { vertices :: [Vector] }

instance Shape Polygon where
  intersections (Polygon vertices) (Ray origin direction) = undefined
  normal (Polygon vertices) vector = undefined
  mapping (Polygon vertices) vector = undefined
