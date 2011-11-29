import PNM.Pnm as PNM

data Polygon = Dreieck Double Double Double

def_pix :: PNM.RGB
def_pix = (140,40,140)

main = uglyrender

uglyrender = do
             let w = 3
             let h = 4
             let screen = (w, h)
             let regions = splitUp 3 4 screen
             --PNM.besuretodelete "out.pnm"
             PNM.writePNMHeader "out.pnm" screen
             render regions
             PNM.saveAsPNG "out.pnm"
             putStrLn "Fertig"

render :: [PNM.Region] -> IO Bool
render [] = return True
render (r:rs) = do
                let pic = trace r
                PNM.writeRegionToFile "out.pnm" pic
                (render rs)

splitUp :: Int -> Int -> PNM.Screen -> [PNM.Region]
splitUp n m (w, h) = [( (px, py), (w,h), xsize, ysize, [[]] ) | py<-[0,ysize .. h], px<-[0,xsize .. w] ]
    where
    ysize = floor ((fromIntegral h)/(fromIntegral m))
    xsize = floor ((fromIntegral w)/(fromIntegral n))

trace :: PNM.Region -> PNM.Region
trace (posi, screen, w, h, _) = (posi, screen, w, h, rows)
                              where
                              rows = colored h
                              colored 0 = []
                              colored n = (replicate w (testfarbe (50+120*((fst posi)+(snd posi))))):(colored (n-1))
testfarbe :: Int -> PNM.RGB
testfarbe c = (fromIntegral(mod c 255), fromIntegral(mod c 255), fromIntegral(mod c 255))
