import qualified Data.ByteString as B

import Color (RGB(..), black, white, red, blue, green)
import Vector (Vector(..), yNormal, zNormal)
import Shape (AnyShape(..))
import Sphere (Sphere(..))
import Plane (Plane(..), fromPoints)
import Triangle (Triangle(..))
import Material(AnyMaterial(..), Diffuse(..))
import Camera (cameraLookAt)
import Tracer (Scene(..), trace)

writePPM :: FilePath -> Integer -> Integer -> [RGB] -> IO ()
writePPM file width height colors = do
  writeFile file $ "P6\n" ++ (show width) ++ " " ++ (show height) ++ "\n255\n"
  B.appendFile file $ B.pack $ concatMap (\(RGB r g b) -> [r, g, b]) colors

main = do
  let (w, h) = (400, 300)
  let sphere1 = (AnyShape $ Sphere (Vector 0 2 15) 3, AnyMaterial $ Diffuse red)
  let sphere2 = (AnyShape $ Sphere (Vector 3 2 5) 2.5, AnyMaterial $ Diffuse blue)
  let plane = (AnyShape $ Plane yNormal (-1), AnyMaterial $ Diffuse green)
  let triangle = (AnyShape $ Triangle (Vector (-1) 2 10) (Vector (-1) (-1) 1) (Vector 0 (-1) 1), AnyMaterial $ Diffuse white)
  let camera = cameraLookAt (Vector 0 0 (-10)) zNormal
  let scene = Scene camera [sphere1, sphere2, plane, triangle] black
  let colors = trace scene w h
  writePPM "test.ppm" w h colors
  putStrLn "Hello, world!"
