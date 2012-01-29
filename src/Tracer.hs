module Tracer(Scene(..), trace) where

import Data.List (minimumBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Function (on)

import Color (RGB)
import Vector (distance2)
import Ray (Ray(..))
import Shape (AnyShape, intersections)
import Material (AnyMaterial, color)
import Camera (Camera, rays)

data Scene = Scene { camera :: Camera, shapes :: [(AnyShape, AnyMaterial)], bgColor :: RGB }

trace :: Scene -> Integer -> Integer -> [RGB]
trace (Scene camera shapes bgColor) targetWidth targetHeight = map (fromMaybe bgColor) [traceSingle ray shapes | ray <- rays camera targetWidth targetHeight]

traceSingle :: Ray -> [(AnyShape, AnyMaterial)] -> Maybe RGB
traceSingle ray shapes = result $ mapMaybe (rayHit ray) shapes
  where result [] = Nothing
        result xs = Just $ snd $ minimumBy (compare `on` fst) xs

rayHit :: Ray -> (AnyShape, AnyMaterial) -> Maybe (Float, RGB)
rayHit ray@(Ray origin _) (shape, material) = result $ map (distance2 origin) $ intersections shape ray
  where result [] = Nothing
        result xs = Just (minimum xs, color material (0, 0))
