module Camera(Camera, camera, cameraLookAt, rays) where

import Prelude hiding (subtract, negate)

import Vector (Vector, yNormal, add, negate, subtract, multiply, cross, normalize)
import Ray (Ray(..), )

data Camera = Camera { location :: Vector, direction :: Vector, up :: Vector, right :: Vector, width :: Integer, height :: Integer, distance :: Float, base :: Vector }

camera :: Vector -> Vector -> Vector -> Integer -> Integer -> Float -> Camera
camera location direction up width height distance = Camera location (normalize direction) (normalize up) (normalize $ cross direction up) width height distance (add location $ multiply direction distance)

cameraLookAt :: Vector -> Vector -> Camera
cameraLookAt location lookAt = camera location (normalize $ subtract lookAt location) yNormal 4 3 1

rays :: Camera -> Integer -> Integer -> [Ray]
rays (Camera location _ up right width height _ base) xPoints yPoints = [Ray v d | y <- [0..(yPoints - 1)], x <- [0..(xPoints - 1)], let v = add start $ add (multiply xOff $ fromInteger x) (multiply yOff $ fromInteger y), let d = normalize $ subtract v location]
  where dw = fromInteger width / fromInteger (xPoints - 1)
        dh = fromInteger height / fromInteger (yPoints - 1)
        xOff = negate $ multiply right dw
        yOff = negate $ multiply up dh
        start = subtract base $ add (multiply xOff (fromInteger (xPoints `div` 2) - fromInteger (1 - xPoints `mod` 2) * 0.5))
                                    (multiply yOff (fromInteger (yPoints `div` 2) - fromInteger (1 - yPoints `mod` 2) * 0.5))
