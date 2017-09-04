module TrifectaLambdaParser() where

import Text.Trifecta
import Data.HashMap.Strict as HM
import Text.Trifecta
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import qualified Data.Vector as V
import Data.Text (Text, pack, unpack)
import Control.Applicative
import Text.Parser.Token.Highlight
import Data.String
import Control.Monad.IO.Class
import Control.Monad(liftM2)


data Expr = Var String  | App Expr Expr | Lambda String Expr
            deriving (Show, Eq)



parseString' :: String -> Result Expr
parseString' = parseString (apply <* eof) mempty

expr :: (Monad m, TokenParsing m) => m Expr
expr =

         variable
         <|> lambda
         <|> (parens apply <?> "parens")


apply :: (Monad m, TokenParsing m) => m Expr
apply = foo <$> ( ws *> (many1 expr) <* ws) <?> "apply"
           where
            foo [x] = x
            foo xs  = foldl1 App xs

many1 p = liftM2 (:) p (many p)

variable :: (Monad m, TokenParsing m) => m Expr
variable = Var <$> variableName <* ws <?> "variable"

variableName :: (Monad m, TokenParsing m) => m String
variableName = many1 (noneOf ".()\\λ \n\t") <?> "variable"

lambda :: (Monad m, TokenParsing m) => m Expr
lambda = Lambda <$> (  (oneOf ['\\', 'λ']) *> ws *> variableName  <* ws <* char '.' <* ws ) <*> (apply <* ws ) <?> "lambda"


ws :: (Monad m, TokenParsing m) => m ()
ws = whiteSpace
