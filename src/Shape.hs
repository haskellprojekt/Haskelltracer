{-# LANGUAGE ExistentialQuantification #-}

module Shape(Shape(..), AnyShape(..)) where

import Vector (Vector)
import Ray (Ray)

class Shape a where
  intersections :: a -> Ray -> [Vector]
  normal :: a -> Vector -> Vector
  mapping :: a -> Vector -> (Float, Float)

data AnyShape = forall a. Shape a => AnyShape a

instance Shape AnyShape where
  intersections (AnyShape a) = intersections a
  normal (AnyShape a) = normal a
  mapping (AnyShape a) = mapping a
