module Main where
import GHC.Conc (numCapabilities)
import Haskelltracer
import Plane
import Triangle
import Pnm
import Math
import Renderer

main = do
       let screen1 = (800,600)
       let conf1 = Config "test.pnm" "test.pov" screen1 (tilesForThreads screen1)
       let cam1 = Camera (Vector 0 1 1) (Vector 4 5 6) (Vector 0 1 0)
       let geom1 = T (Triangle (Vector 2 3 1) (Vector 5 7 5) (Vector 4 5 9) green)
       let testscene = Scene cam1 [geom1]
       putStrLn $ "Starting render process with " ++ (show (length (cTiles conf1))) ++ " Regions"
       let results = render testscene conf1
       writePNMHeader (cOutFile conf1) (cScreen conf1)
       writeAllTiles (cOutFile conf1) results
       saveAsPNG (cOutFile conf1)
       putStrLn "Finished."


writeAllTiles :: String -> [Tile] -> IO ()
writeAllTiles fname (r:rs) = do
                        writeTileToFile fname r
                        if rsisempty rs then return () else  writeAllTiles fname rs
                             where
                               rsisempty [] = True
                               rsisempty [n] = False
                               rsisemptry (n:nn) = False
tilesForThreads :: (Width, Height) -> [Tile]
tilesForThreads screen = regions
                         where
                           (smp1, smp2) = getRegionsForThreads
                           regions = splitUp smp1 smp2 screen
getRegionsForThreads :: (Int, Int)
getRegionsForThreads = (kleinster, div numcore kleinster)
                       where
                       kleinster = getkleinererTeiler (floor (((fromIntegral numcore)+1)/2))
                       numcore = numCapabilities
                       getkleinererTeiler x = if mod numcore x == 0 then x else getkleinererTeiler (x-1)
splitUp :: Int -> Int -> (Width, Height) -> [Tile]
splitUp n m (w, h) = [(Tile (px, py) (w,h) (xsize, ysize) [[]] ) | py<-[0,ysize .. h], px<-[0,xsize .. w], px /= w, py /= h ]
    where
    ysize = floor ((fromIntegral h)/(fromIntegral m))
    xsize = floor ((fromIntegral w)/(fromIntegral n))
