import Monad
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (haskellDef)


--data Vector = Vector Double Double Double deriving Show
--data Object = Sphere Vector Float | Camera Vector Vector deriving Show

type Vector =  (Float, Float, Float)
data Prim = Axis Char | Number Float | Vector Vector deriving Show
type Option = String

data OO = Obj Object | Opt Option deriving Show

data Object = Object String [Prim] [OO] deriving Show

povFile :: GenParser Char st [Object]
povFile =
    do result <- many object
       eof
       return result

object :: GenParser Char st Object
object =
    do
        name <- many (noneOf " \n{")
        spaces
        char '{'
        params <- sepBy parameter (char ',')
        --opts <- sepBy optionOrObject (char '\n')
        char '}'
        return (Object name params [])
        --return (Object name params opts)

parameter :: GenParser Char st Prim
parameter =
    do  l <- letter; return (Axis l)
    <|>
    do  d <- (Number . parseNumber);
--      return (Number d)
        return d
--   <|>
--    ( Vector $ vector )

-- doesn't work for doubles
parseNumber :: GenParser Char st Float
parseNumber = liftM read $ many1 digit

-- vector :: GenParser Char st Vec
--vector = do
--    char '<'; x <- parseNumber
--    char ','; y <- parseNumber
--    char ','; z <- parseNumber
--    char '>'; return (x,y,z)


option1 :: GenParser Char st String
option1 =
    do
        opt <- (many (noneOf "\n"))
        return opt

optionOrObject :: GenParser Char st OO
optionOrObject =
    do
        obj <- try ( object )
        return (Obj obj)
    <|>
    do
        opt <- option1
        return (Opt opt)





parsePov :: String -> Either ParseError [Object]
parsePov input = parse povFile "(unknown)" input

parsePovFile =
    do
        content <- readFile "../test.pov"
        return (parsePov content)

----------------------------------------------
{-
lexer   = P.makeTokenParser haskellDef

object =
    do  string "sphere"
        return ( Sphere ( Vector 1 2 3) 3)
    <|>
    do  string "camera"
        con <- braces cameraObject
        return con


braces handler =
    do  char '{'
        contents <- handler
        char '}'
        return contents

cameraObject =
    do  (location:look_at:[]) <- sepBy cameraKeys (char '\n')
        return ( Camera location look_at)

cameraKeys =
    do  try( string "location")
        char ' '
        v <- vector
        return v
    <|>
    do  string "look_at"
        char ' '
        v <- vector
        return v


vector =
    do  char '<'
        (x:y:z:[]) <- sepBy (P.float lexer) (char ',')
        char '>'
        return (Vector x y z)
-}
