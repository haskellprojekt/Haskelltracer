import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (haskellDef)


data Vector = Vector Double Double Double deriving Show
data Object = Sphere Vector Float | Camera Vector Vector deriving Show

povFile :: GenParser Char st [Object]
povFile =
    do result <- many object
       eof
       return result

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

parsePov :: String -> Either ParseError [Object]
parsePov input = parse povFile "(unknown)" input
