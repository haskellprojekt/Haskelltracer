{-# LANGUAGE ExistentialQuantification #-}

module Shape(Hit(..), Shape(..), AnyShape(..)) where

import Vector (Vector)
import Ray (Ray)

data Hit = Hit {hitDistance :: Float, hitPosition :: Vector}

class Shape a where
  intersection :: a -> Ray -> Maybe Hit
  normal :: a -> Vector -> Vector
  mapping :: a -> Vector -> (Float, Float)

data AnyShape = forall a. Shape a => AnyShape a

instance Shape AnyShape where
  intersection (AnyShape a) = intersection a
  normal (AnyShape a) = normal a
  mapping (AnyShape a) = mapping a
