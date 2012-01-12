module Tracer(Scene(..), trace) where

import Data.List (minimumBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Function (on)

import Color (RGB(..))
import Vector (distance2)
import Ray (Ray(..))
import Shape (withRay, AnyShape)
import Camera (Camera, rays)

data Scene = Scene Camera [(AnyShape, RGB)] RGB

trace :: Scene -> Integer -> Integer -> [RGB]
trace (Scene camera shapes bgColor) xPoints yPoints = map (fromMaybe bgColor) [traceSingle ray shapes | ray <- rays camera xPoints yPoints]

traceSingle :: Ray -> [(AnyShape, RGB)] -> Maybe RGB
traceSingle ray shapes = result $ mapMaybe (rayHit ray) shapes
  where result [] = Nothing
        result xs = Just $ snd $ minimumBy (compare `on` fst) xs

rayHit :: Ray -> (AnyShape, RGB) -> Maybe (Float, RGB)
rayHit ray@(Ray origin _) (shape, color) = result $ map (distance2 origin) $ withRay shape ray
  where result [] = Nothing
        result xs = Just (minimum xs, color)
