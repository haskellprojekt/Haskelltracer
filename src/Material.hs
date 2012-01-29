{-# LANGUAGE ExistentialQuantification #-}

module Material(Material(..), AnyMaterial(..), Diffuse(..)) where

import Color (RGB)
import Vector (Vector)

class Material a where
  color :: a -> (Float, Float) -> RGB
  normal :: a -> (Float, Float) -> Maybe Vector

data AnyMaterial = forall a. Material a => AnyMaterial a

instance Material AnyMaterial where
  color (AnyMaterial a) = color a
  normal (AnyMaterial a) = normal a

data Diffuse = Diffuse RGB

instance Material Diffuse where
  color (Diffuse rgb) _ = rgb
  normal _ _ = Nothing
