--module Camera(Camera, camera, cameraLookAt, rays) where
module Camera(Camera(..), cameraLookAt, rays) where

import Prelude hiding (subtract, negate)

import Vector (Vector, yNormal, add, negate, subtract, multiply, cross, normalize)
import Ray (Ray(..), )

--data Camera = Camera { location :: Vector, direction :: Vector, up :: Vector, right :: Vector, width :: Integer, height :: Integer, distance :: Float, base :: Vector }

data Camera = Camera { location :: Vector, direction :: Vector, up :: Vector, fieldOfView :: Float, aspectRatio :: Float, distance :: Float }

--camera :: Vector -> Vector -> Vector -> Integer -> Integer -> Float -> Camera
--camera location direction up width height distance = Camera location (normalize direction) (normalize up) (normalize $ cross up direction) width height distance (add location $ multiply direction distance)

--cameraLookAt :: Vector -> Vector -> Camera
--cameraLookAt location lookAt = camera location (normalize $ subtract lookAt location) yNormal 4 3 1

cameraLookAt :: Vector -> Vector -> Camera
cameraLookAt location lookAt = Camera location (normalize $ subtract lookAt location) yNormal 60 (4/3) 0.1

--rays :: Camera -> Integer -> Integer -> [Ray]
--rays (Camera location _ up right width height _ base) xPoints yPoints = [Ray v d | y <- [0..(yPoints - 1)], x <- [0..(xPoints - 1)], let v = add start $ add (multiply xOff $ fromInteger x) (multiply yOff $ fromInteger y), let d = normalize $ subtract v location]
--  where dw = fromInteger width / fromInteger (xPoints - 1)
--        dh = fromInteger height / fromInteger (yPoints - 1)
--        xOff = multiply right dw
--        yOff = negate $ multiply up dh
--        start = subtract base $ add (multiply xOff (fromInteger (xPoints `div` 2) - fromInteger (1 - xPoints `mod` 2) * 0.5))
--                                    (multiply yOff (fromInteger (yPoints `div` 2) - fromInteger (1 - yPoints `mod` 2) * 0.5))

rays :: Camera -> Integer -> Integer -> [Ray]
rays (Camera location direction up fieldOfView aspectRatio distance) targetWidth targetHeight = [Ray v d | y <- [0..(targetHeight - 1)], x <- [0..(targetWidth - 1)], let v = add start $ add (multiply xOff $ fromInteger x) (multiply yOff $ fromInteger y), let d = normalize $ subtract v location]
  where tanFieldOfView = tan(fieldOfView*pi/360) -- tan(fieldOfView*pi/180/2)
        width = 2*tanFieldOfView*distance
        height = width/aspectRatio
        dw = width / fromInteger (targetWidth - 1)
        dh = height / fromInteger (targetHeight - 1)
        right = normalize $ cross up direction
        base = add location $ multiply direction distance
        xOff = multiply right dw
        yOff = negate $ multiply up dh
        left = negate $ multiply right (width/2)
        top = multiply up (height*0.5)
        start = add base $ add top left
