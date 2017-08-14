module Lambda(
        Expr(..)
      , parseExpression
      , parseOrFail
      , beta
      , beta'
      , beta''
      , traceOrFail


) where

import Text.Parsec (ParseError)
import Text.Parsec.Token(lexeme)
import Text.Parsec.String (Parser)
import Text.Parsec.Prim (parse, try, (<?>))
import Text.Parsec.Char (oneOf, char, digit, letter, satisfy, string)
import Text.Parsec.Combinator (many1, chainl1, between, eof, optionMaybe,sepBy, notFollowedBy, anyToken)
import Control.Applicative ((<$>), (<**>), (<*>), (<*), (*>), (<|>), many, (<$))
import Control.Monad (void, ap)
import Data.Char (isLetter, isDigit)
import qualified Data.Vector as V
import Data.Char(digitToInt)
import Data.List(delete, union, find)
import Debug.Trace(trace, traceShow, traceShowId)
import Control.Monad.Writer(Writer,tell)
import qualified Control.Monad.Writer as Writer
--import Control.Applicative

data Variable = Variable Char


data Cmd = Help 

data Expr = Var Char  | App Expr Expr | Lambda Char Expr | Val String -- | const
            deriving (Show, Eq)



parseOrFail :: String -> Expr
parseOrFail s =  either (error . show)  id (regularParse expr'' s)

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""


parseExpression :: String -> Either ParseError Expr
parseExpression  = regularParse expr''

expr'' :: Parser Expr
expr'' = expr' <* ws <* eof

expr' :: Parser Expr
expr' =  apply

pexpr' :: Parser Expr
pexpr' = parens expr'



explodeApp :: [Expr] -> Expr
explodeApp = foldl1 (\a -> \r -> App a r)

apply :: Parser Expr
apply =
   ((,) <$> (expr <* ws) <*> (expr `sepBy` ws) ) >>= ( \foo ->
      case foo of
         (e1, []) -> return $ e1
         (e1, xs) -> return $ (explodeApp $ e1:xs)
   )


expr :: Parser Expr
expr =
        lambda
        <|> variable
        <|> (parens expr')
        -- left factoring !! rubbish ))

{-
expr :: Parser Expr
expr =
        lambda
        <|>  variable
        <|> parens app -- left factoring !! doesn't work both have parens!
-}

valP :: Parser Expr
valP = fmap Val $ (:) <$> oneOf ['A'..'Z'] <*> many (oneOf $ '_':'\'':['A'..'Z']++['a'..'z']++['0'..'9'] ) -- FIXME: efficiency

---- ATTENTION conflict in the beginning of line "A" - val definition or denomination?
valDef :: Parser (String, Expr)
valDef = (,) <$> ((fmap getValName valP) <* ws <* char '=' <* ws) <*> expr''



getValName :: Expr -> String
getValName (Val name) = name
getValName _ = error "trying to get name of Expr"

app :: Parser Expr
app =  App <$> (expr' <* ws') <*> (expr')

lambda :: Parser Expr
lambda =  Lambda <$> (  lambdaLit *> ws *> letter <* ws <* char '.' <* ws) <*> (expr' <* ws )

lambdaLit :: Parser Char
lambdaLit = oneOf ['\\', 'λ', '+']

variable :: Parser Expr
variable = fmap Var letter

--constant :: Parser Expr
--constant = fmap (Const . digitToInt) digit




parens :: Parser a -> Parser a
parens p =  char '(' *> ws *>  p <* ws <*  char ')'


--parens' = between (char '(') (char ')')
-- parens' :: Parser a -> Parser a
-- parens' p = do
--     void $ char '('
--     e <- p
--     void $ char ')'
--     return e




ws :: Parser ()
ws = void $ many $ oneOf " \n\t"

ws' :: Parser ()
ws' = void $ many1 $ oneOf " \n\t"


------------------- reduction -------------------------

-- FIXME: set
freeV :: Expr -> [Char]
freeV (Var x) = [x]
freeV (Lambda x t) = delete x $ freeV t
freeV (App e1 e2) = union (freeV e1) (freeV e2)


alpha:: Char -> Char -> Expr -> Expr
alpha x z (Var y) | x == y    = Var z
                  | otherwise =  Var y
alpha x z (App a b) = App (alpha x z a) (alpha x z b)
alpha x z (Lambda y f) | x == y = Lambda y f -- ???
                       | otherwise = if elem z (freeV f)
                                     then
                                       error "x in FV (f)"
                                     else
                                      Lambda y (alpha x z f)

subst :: Char -> Expr -> Expr -> Expr
subst c e2 v@(Var y) | c == y    = e2
                     | otherwise = v
subst c e2 (App a b)  = App (subst c e2 a) (subst c e2 b)
subst c e2 l@(Lambda y f) | c == y = l
                          | otherwise = if elem c (freeV e2)
                                        then
                                          error $ "error in subst: '" ++ [c] ++ "' in FW " ++ (show e2)
                                        else
                                          Lambda y (subst c e2 f)


needsAlpha :: Expr -> Expr -> Bool
needsAlpha e2 (Lambda y _) = elem (y) (freeV e2)
needsAlpha _ _             = False

fixFreeVars :: Expr -> Expr -> (Char, Expr)
fixFreeVars e2 (Lambda x e1) = (s, alpha x s e1)
   where
     symbols = ['a'..'z']
     fv      = freeV e2
     s       = maybe (error "not enough vars!") id $ find (\z -> not $ elem z fv) symbols


isLambda :: Expr -> Bool
isLambda (Lambda _ _) = True
isLambda _ = False

-- http://www.cs.cornell.edu/courses/cs6110/2014sp/Handouts/Sestoft.pdf

-- same as subst, but will invoke alpha if needed

-- FIXME: trace, single step

subst' :: Expr -> Expr -> Expr
subst' l@(Lambda v e1) e2 = if needsAlpha e2 l then
                              let (v', e1') = fixFreeVars e2 l
                              in subst v' e2 e1'
                            else
                               subst v e2 e1

-- call-by-name, computes weak head normal form
cbn :: Expr -> Expr
cbn v@(Var _) = v
cbn l@(Lambda _ _) = l
cbn (App e1 e2) = case cbn e1 of
                    l@(Lambda x e) -> cbn $ subst' l e2
                    e1'          -> App e1' e2


-- normal order reduction
beta :: Expr -> Expr
beta (App e1 e2) = case cbn e1 of
                    l@(Lambda x e) -> beta $ subst' l e2
                    e1'            -> let e1'' = beta e1'
                                      in App e1'' (beta e2)
beta v@(Var _) = v
beta (Lambda v e) = Lambda v $ beta e


type Ctxt = Expr -> Expr

lambdaC x = Lambda x
app2C e1  = App e1
app1C e2  = flip (App) e2

substc :: Ctxt -> Expr -> Expr -> Writer [String] Expr
substc ctxt l@(Lambda v e1) e2 = if  needsAlpha e2 l then
                              let (v', e1') = fixFreeVars e2 l
                              in do
                                 tell $ [pprint $ ctxt $ App l e2]
                                 return $ subst v' e2 e1'
                            else do
                                 tell $ [pprint $ ctxt $ App l e2]
                                 return $ subst v e2 e1

cbn' :: Ctxt -> Expr -> Writer [String] Expr
cbn' ctxt v@(Var _) = return v
cbn' ctxt l@(Lambda _ _) = return l
cbn' ctxt (App e1 e2) = (cbn' (ctxt . app1C e2) e1) >>= \e1' ->
                          case e1' of
                            l@(Lambda x e) -> substc ctxt l e2 >>= (cbn' ctxt)
                            _          -> return $ App e1' e2

beta' :: Ctxt -> Expr -> Writer [String] Expr
beta' ctxt (App e1 e2) =  (cbn' (ctxt . app1C e2) e1) >>= \e1' ->
                             case e1' of
                               l@(Lambda x e) -> (beta' (ctxt))  =<< substc ctxt l e2
                               _              -> App <$> (beta' (ctxt . app1C e2) e1') <*> (beta' (ctxt . app2C e1') e2)
beta' ctxt (Lambda v e) = fmap (Lambda v) $ beta' (ctxt . lambdaC v) e
beta' ctxt v@(Var _) = return v





runTestA = do
           sequence $ fmap (\x -> putStrLn $ "==> " ++ x) xs
           putStrLn $ "==> " ++ pprint r
  where
     (r, xs) = Writer.runWriter $ beta' id $ parseOrFail "(λn.λf.λx.f (n f x)) (λf.λx.f (f x))"

runTestB s = do
           sequence $ fmap (\x -> putStrLn $ "==> " ++ x) xs
           putStrLn $ "==> " ++ pprint r
  where
     (r, xs) = Writer.runWriter $ beta' id $ parseOrFail s

traceOrFail :: String -> String
traceOrFail s = either (\err -> "Сорян, хуйня какя-то. " ++ show err) id myres
  where
    maybeExpr = parseExpression s
    myres     = fmap eval maybeExpr
    eval ex   = concat $ fmap (\x -> "\n==> " ++ x) res
      where
        (r, xs) = Writer.runWriter $ beta' id $ ex
        rs = pprint r
        res = xs ++ [rs]



------------ FIXME: beta, consecutive apply ----------------------------


-- pprint :: Expr -> String
-- pprint (Var a) = [a]
-- pprint (App a b) =    pprint a ++ " " ++ rst
--            where
--              rst = case b of
--                    Var a -> [a]
--                    _     -> "(" ++ pprint b ++")"
-- pprint (Lambda a b) = "(" ++ "\\" ++ [a] ++ "." ++ pprint b ++ ")"

pprint :: Expr -> String
pprint (Var a) = [a]
pprint (App a b@(App _ _)) = pprint a ++ " " ++ "(" ++ pprint b ++ ")"
pprint (App a@(App _ _) b) =    pprint a ++ " " ++ pprint b
pprint (App l@(Lambda _ _) b) = "(" ++ pprint l ++ ")" ++ " " ++ pprint b
pprint (App a b) = "" ++ pprint a ++ " " ++ pprint b ++ ""
pprint (Lambda x b) =  "λ" ++ [x] ++ "." ++ pprint b
--pprint (Lambda x b) = "\\" ++ [x] ++ "." ++ "(" ++ pprint b ++ ")"

test0 = "\\f.\\x.f ((\\n.\\f.\\x.f (n f x)) (\\f.\\x.x) f x)"
test0Expr = Lambda 'f' (Lambda 'x' (App (Var 'f') (App (App (App (Lambda 'n' (Lambda 'f' (Lambda 'x' (App (Var 'f') (App (App (Var 'n') (Var 'f')) (Var 'x')))))) (Lambda 'f' (Lambda 'x' (Var 'x')))) (Var 'f')) (Var 'x'))))

test1 = "(\\a.\\b.a (b (\\n.\\f.\\x.f (n f x))) (\\f.\\x.x)) (\\f.\\x.f (f (f x))) (\\f.\\x.f (f x))"
test1E = App (App (Lambda 'a' (Lambda 'b' (App (App (Var 'a') (App (Var 'b') (Lambda 'n' (Lambda 'f' (Lambda 'x' (App (Var 'f') (App (App (Var 'n') (Var 'f')) (Var 'x')))))))) (Lambda 'f' (Lambda 'x' (Var 'x')))))) (Lambda 'f' (Lambda 'x' (App (Var 'f') (App (Var 'f') (App (Var 'f') (Var 'x'))))))) (Lambda 'f' (Lambda 'x' (App (Var 'f') (App (Var 'f') (Var 'x')))))
-------------------- write interpreter --------------------------------
-- "(((\\x y.(y x))(((((\\x y. (y x))(((\\x.x) 12)))) (\\x.x))))(\\x.x))"
-- "((\\f.\\x.f (f (f x))) ((\\f.\\x.f (f x)) (\\n.\\f.\\x.f ((n f) x)))) (\\f.\\x.x)"
performReduction :: Expr -> [Expr]
performReduction e = if e' == e
                     then [e]
                     else e': performReduction e'
     where
       e' = beta e

interpreter :: IO ()
interpreter = do
               input <- getLine
               let res = case regularParse expr' input of
                          (Left err) ->   "fail. " ++ (show err)
                          (Right expr) -> "ok"
               putStrLn res


--------------------------------------------------------------------------------
----------------------- single step --------------------------------------------
--------------------------------------------------------------------------------


type Exception e a = Either e a

throw :: e -> Exception e a
throw = Left



-- substc :: Ctxt -> Expr -> Expr -> Writer [String] Expr
-- substc ctxt l@(Lambda v e1) e2 = if  needsAlpha e2 l then
--                               let (v', e1') = fixFreeVars e2 l
--                               in do
--                                  tell $ [pprint $ ctxt $ App l e2]
--                                  return $ subst v' e2 e1'
--                             else do
--                                  tell $ [pprint $ ctxt $ App l e2]
--                                  return $ subst v e2 e1



substd :: Ctxt -> Expr -> Expr -> Exception Expr Expr
substd ctxt l@(Lambda v e1) e2 = if  needsAlpha e2 l then
                              let (v', e1') = fixFreeVars e2 l
                              in do
                                --  tell $ [pprint $ ctxt $ App l e2]
                                 throw $ ctxt $ subst v' e2 e1'
                            else do
                                --  tell $ [pprint $ ctxt $ App l e2]
                                 throw $ ctxt $ subst v e2 e1

cbn'' :: Ctxt -> Expr -> Exception Expr Expr
cbn'' ctxt v@(Var _) = return v
cbn'' ctxt l@(Lambda _ _) = return l
cbn'' ctxt (App e1 e2) = (cbn'' (ctxt . app1C e2) e1) >>= \e1' ->
                          case e1' of
                            l@(Lambda x e) -> substd ctxt l e2 >>= (cbn'' ctxt)
                            _          -> return $ App e1' e2

beta'' :: Ctxt -> Expr -> Exception Expr Expr
beta'' ctxt (App e1 e2) =  (cbn'' (ctxt . app1C e2) e1) >>= \e1' ->
                             case e1' of
                               l@(Lambda x e) -> (beta'' (ctxt))  =<< substd ctxt l e2
                               _              -> App <$> (beta'' (ctxt . app1C e2) e1') <*> (beta'' (ctxt . app2C e1') e2)
beta'' ctxt (Lambda v e) = fmap (Lambda v) $ beta'' (ctxt . lambdaC v) e
beta'' ctxt v@(Var _) = return v

runTestS s = do
           putStrLn $ "==> " ++ pprint res
  where
     res = either id id $ beta'' id $ parseOrFail s


--  "(λn.λf.λx.f (n f x)) (λf.λx.f (f x))"
