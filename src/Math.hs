module Math where
import Haskelltracer



-- ugly:
-- t: stützvektor; r: richtungsvektor der Geraden.
-- a, b, c: Stützvektoren der Ecken des Dreiecks.
-- zu lösende Gleichung: t + x*r = a + u*(b-a) + v*(c-a)
getSolutionUV :: (Vector, Vector) -> (Vector, Vector, Vector) -> (Float, Float, Float)
getSolutionUV (t, r) (a, b, c) = (x, u, v)
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
    getV n (Vector g h i) = [g,h,i] !! (n-1)
    lin l = [getV l r, (getV l (b - a) )*(-1), (getV l (c - a) )*(-1), (getV l a) - (getV l t)]
    mal xl i = map (i*) xl
    minus xl1 xl2 = zipWith (-) xl1 xl2


instance Num Vector where
  (+) (Vector a b c) (Vector x y z) = Vector (a+x) (b+y) (c+z)
  fromInteger i = Vector (fromInteger i) (fromInteger i) (fromInteger i)
  -- because of fromInteger: (Vector 1 1 1) == 1 :: Vector
  (-) (Vector a b c) (Vector x y z) = Vector (a-x) (b-y) (c-z)
  abs _ = error "abs is not defined for a vector"
  (*) _ = error "(*) is not defined for a vector"
  signum _ = error "signum is not defined for a vector"

crossproduct :: Vector -> Vector -> Vector
crossproduct (Vector a b c) (Vector x y z) = Vector (b*z - c*y) (c*x - a*z) (a*y - b*x)

scalarproduct :: Vector -> Vector -> Float
scalarproduct (Vector a b c) (Vector x y z) = a*x + b*y + c*z

vangle :: Vector -> Vector -> Float
vangle a b = asin ((vlen (a `crossproduct` b)) / ((vlen a)*(vlen b))) * 180.0 / pi

vlen :: Vector -> Float
vlen (Vector a b c) = sqrt(a*a + b*b + c*c)

vmult :: Vector -> Float -> Vector
vmult (Vector a b c) i = Vector (a*i) (b*i) (c*i)

vdiv :: Vector -> Float -> Vector
vdiv (Vector a b c) i = Vector (a/i) (b/i) (c/i)

unitVector :: Vector -> Vector
unitVector a = a `vdiv` (vlen a)
