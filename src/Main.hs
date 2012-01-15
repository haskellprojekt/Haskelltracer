import qualified Data.ByteString as B

import Color (RGB(..), black, white, red, blue, green)
import Vector (Vector(..), yNormal)
import Shape (AnyShape(..))
import Sphere (Sphere(..))
import Plane (Plane(..), fromPoints)
import Triangle (Triangle(..))
import Camera (cameraLookAt)
import Tracer (Scene(..), trace)

writePPM :: FilePath -> Integer -> Integer -> [RGB] -> IO ()
writePPM file width height colors = do
  writeFile file $ "P6\n" ++ (show width) ++ " " ++ (show height) ++ "\n255\n"
  B.appendFile file $ B.pack $ concatMap (\(RGB r g b) -> [r, g, b]) colors

main = do
  let (w, h) = (400, 300)
  let sphere1 = (AnyShape $ Sphere (Vector 0 2 5) 3, red)
  let sphere2 = (AnyShape $ Sphere (Vector 3 0 5) 2.5, blue)
  let plane = (AnyShape $ Plane yNormal (-1), green)
  let triangle = (AnyShape $ Triangle (Vector (-1) 2 1) (Vector (-1) (-1) 1) (Vector 0 (-1) 1), white)
  let camera = cameraLookAt (Vector 0 0 0) (Vector 0 0 5)
  let scene = Scene camera [sphere1, sphere2, plane, triangle] black
  let colors = trace scene w h
  writePPM "test.ppm" w h colors
  putStrLn "Hello, world!"
