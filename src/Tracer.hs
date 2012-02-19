module Tracer(Scene(..), trace) where

import Prelude hiding (subtract)

import Data.List (sortBy, minimumBy)
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe)
import Data.Function (on)

import Utils (maybeZip)
import Color (RGB(..))
import Vector (Vector, subtract, dot, distance2, normalize)
import Ray (Ray(..))
import Shape (Hit(..), AnyShape, intersection, normal)
import Material (AnyMaterial, color)
import Light (Light(..))
import Camera (Camera, rays)

data Scene = Scene { camera :: Camera, shapes :: [(AnyShape, AnyMaterial)], lights :: [Light], bgColor :: RGB }

--trace' :: Scene -> Integer -> Integer -> [RGB]
--trace' (Scene camera shapes lights bgColor) targetWidth targetHeight = map (fromMaybe bgColor) [traceSingle' ray shapes | ray <- rays camera targetWidth targetHeight]

--traceSingle' :: Ray -> [(AnyShape, AnyMaterial)] -> Maybe RGB
--traceSingle' ray shapes = result $ mapMaybe (rayHit' ray) shapes
--  where result [] = Nothing
--        result xs = Just $ snd $ minimumBy (compare `on` fst) xs

--rayHit' :: Ray -> (AnyShape, AnyMaterial) -> Maybe (Float, RGB)
--rayHit' ray@(Ray origin _) (shape, material) = result $ map (distance2 origin) $ map hitPosition $ intersection shape ray
--  where result [] = Nothing
--        result xs = Just (minimum xs, color material (0, 0))


trace :: Scene -> Integer -> Integer -> [RGB]
trace (Scene camera shapes lights bgColor) targetWidth targetHeight =
  map (fromMaybe bgColor) [traceSingle ray shapes lights | ray <- rays camera targetWidth targetHeight]

traceSingle :: Ray -> [(AnyShape, AnyMaterial)] -> [Light] -> Maybe RGB
traceSingle ray shapes lights = result hit
  where hit = rayHit ray shapes
        result Nothing = Nothing
        result (Just h@(v, s, m)) = Just $ (\(RGB r g b) -> RGB (truncate (fromIntegral r * shadedR)) (truncate (fromIntegral g * shadedG)) (truncate (fromIntegral b * shadedB))) $ color m (0, 0) -- $ mapping s v
          where (shadedR, shadedG, shadedB) = foldl (\(r, g, b) l -> let (sr, sg, sb) = shadow ray h l shapes in (min 1 $ r + sr, min 1 $ g + sg, min 1 $ b + sb)) (0, 0, 0) lights

rayHit :: Ray -> [(AnyShape, AnyMaterial)] -> Maybe (Vector, AnyShape, AnyMaterial)
rayHit ray shapes = listToMaybe $ map (\(_, p, s, m) -> (p, s, m)) $ sortBy (compare `on` (\(d, _, _, _) -> d)) $ maybeZip (\(s, m) (Hit d p) -> (d, p, s, m)) shapes [intersection shape ray | (shape, _) <- shapes]

--hit :: Ray -> (AnyShape, AnyMaterial) -> [Light]
--traceRay 

shadow :: Ray -> (Vector, AnyShape, AnyMaterial) -> Light -> [(AnyShape, AnyMaterial)] -> (Float, Float, Float)
shadow ray (v, s, _) (PointLight l _ (RGB r g b)) _ = (fromIntegral r * n / 255, fromIntegral g * n / 255, fromIntegral b * n / 255)
  where n = max 0 $ dot (normal s v) (normalize $ subtract l v)

--for each ray
--  for each shape
--    hit = intersection shape ray
--
--    if hit
--      for each light
