{-# LANGUAGE ExistentialQuantification #-}

module Shape(Shape(..), AnyShape(..)) where

import Vector (Vector)
import Ray (Ray)

class Shape a where
  withRay :: a -> Ray -> [Vector]

data AnyShape = forall a. Shape a => AnyShape a

instance Shape AnyShape where
  withRay (AnyShape a) = withRay a
