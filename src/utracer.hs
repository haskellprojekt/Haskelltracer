import PNM.Pnm as PNM
import Control.Parallel
import Control.Parallel.Strategies
import Control.DeepSeq
import GHC.Conc (numCapabilities)

-- @TODO: * zum optimieren lokale definitionen rausmachen
-- * doch Float?
-- * alles andere als Dreiecke, Liste von Welt usw.

type Punkt = (Double, Double, Double)
data Vektor = V Punkt
              deriving (Show)

class Vek v where
  vdiv :: v -> Double -> v
  vmult :: v -> Double -> v
  vsprod :: v -> v -> Double
  vxv :: v -> v -> v
  len :: v -> Double
  winkel :: v -> v -> Double
  einheitsvektor :: v -> v
  vplus :: v -> v -> v
  vminus :: v -> v -> v

instance Vek Vektor where
  (V (a,b,c)) `vdiv` (i) = V (a/i,b/i,c/i)
  (V (a,b,c)) `vsprod` (V (x,y,z)) = a*x + b*y + c*z
  (V (a,b,c)) `vxv` (V (x,y,z)) = V (b*z - c*y, c*x - a*z, a*y - b*x)
  len (V (a,b,c)) = sqrt(a*a + b*b + c*c)
  winkel (V a) (V b) = asin(len((V a) `vxv` (V b)) / ((len (V a))*(len (V b)))) * 180 / pi
  einheitsvektor (V a) = ((V a) `vdiv` (len (V a)))
  (V (a,b,c)) `vplus` (V (x,y,z)) = V (a+x,b+y,c+z)
  (V (a,b,c)) `vminus` (V (x,y,z)) = V (a-x, b-y, c-z)
  (V (a,b,c)) `vmult` (i) = V (a*i,b*i,c*i)


data  Objekt = Dreieck Vektor Vektor Vektor
               | Gerade Vektor Vektor

testeck = Dreieck (V (2,3,1)) (V (5,7,5)) (V (4,5,9))
campoint = V (0,1,1) -- standort der kamera
blickzu = V (4,5,6) -- kann in Ferne liegen
richtung = (einheitsvektor (blickzu `vminus` campoint)) `vmult` 5
leinwagex = V (0, -8, 0)   -- müsste gedreht werden: ist aber von der länge fix -- -5
leinwagey = V (0, 0, 6)  -- müsste gedreht werden: von länge fix -- 3.5

getLeinPunkt :: Double -> Double -> Vektor
getLeinPunkt w h = (startp `vplus` (lx `vmult` (lw*w))) `vplus` (ly `vmult` (lh*h))
                   where
                   startp = mittelp `vminus` ((leinwagex `vplus` leinwagey) `vdiv` 2)
                   mittelp = campoint `vminus` richtung
                   lx = einheitsvektor leinwagex
                   ly = einheitsvektor leinwagey
                   lw = len leinwagex
                   lh = len leinwagey

getLeinVekt :: Double -> Double -> Vektor
getLeinVekt w h = campoint `vminus` (getLeinPunkt w h)

-- x>0 heißt nur "nach vorne" strahlen
-- u und v müssen dreieck geben
-- wenn u,v oder x == Nan also z /= z, dann auch falsch
hasSchnittpunkt :: Objekt -> Objekt -> Bool
hasSchnittpunkt (Dreieck a b c) (Gerade st richt) = if u < 0 || v < 0 || (u+v) > 1 || x < 0 || u /= u || v /= v || x /= x then False else True
  where
    (x, u, v) = getLoesungUV (st, richt) (a, b, c)

-- t: stützvektor; r: richtungsvektor der Geraden. a, b, c: Stützvektoren der Ecken des Dreiecks.
-- zu lösende Gleichung: t + x*r = a + u*(b-a) + v*(c-a)
getLoesungUV :: (Vektor, Vektor) -> (Vektor, Vektor, Vektor) -> (Double, Double, Double)
getLoesungUV (t, r) (a, b, c) = (x, u, v)
  where
    x = ((schritt6 !! 0) !! 3)/((schritt6 !! 0) !! 0)
    v = ((schritt3 !! 2) !! 3)/((schritt3 !! 2) !! 2)
    u = ((schritt4 !! 1) !! 3)/((schritt4 !! 1) !! 1) -- nimmt spalte (3+1) in zeile (1+1) und teilt durch wert in (1+1)-ter spalte in zeile (1+1)
    schritt6 = systemupdate schritt5 (rmultmin (glnr schritt5 1) (glnr schritt5 3) 2) 1 -- erste Zeile v weg (da in 3.zeile alleinstehend)
    schritt5 = systemupdate schritt4 (rmultmin (glnr schritt4 1) (glnr schritt4 2) 1) 1 -- erste Zeile u weg (also z1 - z2 nachdem beides mal jew. anderer wert in spalte (1+1))
    schritt4 = systemupdate schritt3 (rmultmin (glnr schritt3 2) (glnr schritt3 3) 2) 2         -- zweite zeile v weg
    schritt3 = systemupdate schritt2 (rmultmin (glnr schritt2 3) (glnr schritt2 2) 1) 3      -- dritte zeile u weg
    schritt2 = systemupdate schritt1 (rmultmin (glnr schritt1 2) (glnr schritt1 1) 0) 2      -- zweite zeile x weg
    schritt1 = systemupdate ((lin 1):(lin 2):(lin 3):[]) (rmultmin (lin 3) (lin 1) 0) 3    -- unten x weg
    systemupdate sys newline zeile = (take (zeile - 1) sys) ++ [newline] ++ (drop zeile sys) -- im GLsys wird newline anstelle der zeile-ten Zeile eingesetzt
    glnr xl n = xl !! (n-1)
    rmultmin ob un sp = (ob `mal` (un !! sp)) `minus` (un `mal` (ob !! sp))     -- eleminiert in (sp+1)-ten Spalte der Zeile ob die variable, wenn sie auch in un != null ist
    getV n (V (g,h,i)) = [g,h,i] !! (n-1)
    lin l = [getV l r, (getV l (b `vminus` a) )*(-1), (getV l (c `vminus` a) )*(-1), (getV l a) - (getV l t)]
    mal xl i = map (i*) xl
    minus xl1 xl2 = zipWith (-) xl1 xl2




def_pix :: PNM.RGB
def_pix = (140,40,140)

main = uglyrender

uglyrender = do
             let w = 800
             let h = 600
             let screen = (w, h)
             let (smp1, smp2) = getRegionsForThreads
             let regions = splitUp smp1 smp2 screen
             putStrLn ((show (length regions)) ++ " Regionen: " ++ (show smp1) ++ "*" ++ (show smp2))
             PNM.writePNMHeader "out.pnm" screen
             let rendered = render regions
             writeAll rendered "out.pnm"
             PNM.saveAsPNG "out.pnm"
             putStrLn "Fertig"

getRegionsForThreads :: (Int, Int)
getRegionsForThreads = (kleinster, div numcore kleinster)
                       where
                       kleinster = getkleinererTeiler (floor (((fromIntegral numcore)+1)/2))
                       numcore = numCapabilities
                       getkleinererTeiler x = if mod numcore x == 0 then x else getkleinererTeiler (x-1)

writeAll :: [PNM.Region] -> String -> IO ()
writeAll (r:rs) fname = do
                        PNM.writeRegionToFile fname r
                        if rs == [] then return () else  writeAll rs fname

render :: [PNM.Region] -> [PNM.Region]
render regions = (parMap rdeepseq) trace regions
-- alternativen: rdeepseq rseq rpar (alt rwhnf)

splitUp :: Int -> Int -> PNM.Screen -> [PNM.Region]
splitUp n m (w, h) = [( (px, py), (w,h), xsize, ysize, [[]] ) | py<-[0,ysize .. h], px<-[0,xsize .. w], px /= w, py /= h ]
    where
    ysize = floor ((fromIntegral h)/(fromIntegral m))
    xsize = floor ((fromIntegral w)/(fromIntegral n))

trace :: PNM.Region -> PNM.Region
trace (posi, screen, w, h, _) = (posi, screen, w, h, rows)
                              where
                                rows = makeLines 1
                                makeLines z
                                 | z == h = (makeRow 1 h):[]
                                 | otherwise = (makeRow 1 z):(makeLines (z+1))
                                makeRow s y
                                 | s == w = (getPixelOn ((fst posi) + w-1) ((snd posi) + y-1) (fst screen) (snd screen)):[]
                                 | otherwise = (getPixelOn ((fst posi) + s-1) ((snd posi) + y-1) (fst screen) (snd screen)):(makeRow (s+1) y)

getPixelOn :: Int -> Int -> Int -> Int -> PNM.RGB
getPixelOn a b sw sh = dreitreffen
  where
    dreitreffen = if hasSchnittpunkt testeck (Gerade ((getLeinPunkt x y) `vplus` (getLeinVekt x y)) (getLeinVekt x y)) then (200,255,0) else def_pix
    x = (fromIntegral a)/(fromIntegral sw)
    y = (fromIntegral b)/(fromIntegral sh)
