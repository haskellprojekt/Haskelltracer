module Polygon(Polygon(..)) where

data Polygon = Polygon { vertices :: [Vector] }

instance Shape Polygon where
  withRay (Polygon vertices) (Ray origin direction) = undefined
