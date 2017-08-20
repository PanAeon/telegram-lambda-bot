module Shlambda(
        Expr(..)
      , parseExpression
      , regularParse
      , parseOrFail
      , valP
      , valDef
      , beta'''
      , looksLikeValueDef
      , traceOrFail'''
      , basicVals
      , isRecursive
      , pprint

) where


-- FIXME: check message size for telegram

-- Buildpack :: https://github.com/mfine/heroku-buildpack-stack

-- https://core.telegram.org/bots/api#sendmessage


-- TODO: ghci debugger
-- http://projects.haskell.org/hat/
-- https://wiki.haskell.org/Hoed
-- GHC.Stack
-- https://simonmar.github.io/posts/2016-02-12-Stack-traces-in-GHCi.html
-- https://github.com/commercialhaskell/stack/issues/2358
-- https://ocharles.org.uk/blog/posts/2016-01-26-transformers-free-monads-mtl-laws.html

-- TODO: checkout http://hackage.haskell.org/package/haskeline
-- TODO: see also https://stackoverflow.com/questions/21114222/difference-in-getline-functionality-with-ghci-vs-runhaskell

-- checkout http://hackage.haskell.org/package/LambdaCalculator-0.2/src/LambdaCalculator.hs


import Text.Parsec (ParseError)
import Text.Parsec.Token(lexeme)
import Text.Parsec.String (Parser)
import Text.Parsec.Prim (parse, try, (<?>))
import Text.Parsec.Char (oneOf, char, digit, letter, satisfy, string)
import Text.Parsec.Combinator (many1, chainl1, between, eof, optionMaybe,sepBy, notFollowedBy, anyToken)
import Control.Applicative ((<$>), (<**>), (<*>), (<*), (*>), (<|>), many, (<$))
import Control.Monad (void, ap, liftM2)
import Data.Char (isLetter, isDigit)
import qualified Data.Vector as V
import Data.Char(digitToInt)
import Data.List(delete, union, find)
import Debug.Trace(trace, traceShow, traceShowId)
import Control.Monad.Writer(Writer,tell)
import Control.Monad.Trans.State.Lazy
import qualified Control.Monad.Writer as Writer
--import Control.Applicative
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.Except
import Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as HM

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


looksLikeValueDef :: String -> Bool
looksLikeValueDef s = either (const False) (const True) (regularParse (valP <* ws <* char '=' <* ws) s)

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
        <|> valP
        <|> (parens expr')




valP :: Parser Expr
valP = fmap Val $ (:) <$> oneOf ('\'':['A'..'Z']) <*> many (oneOf $ '_':'\'':['A'..'Z']++['a'..'z']++['0'..'9']++['+','-','=', '/', '*', '<', '>', '?'] ) -- FIXME: efficiency


valDef :: Parser (String, Expr)
valDef = (,) <$> ((fmap getValName valP) <* ws <* char '=' <* ws) <*> expr''



getValName :: Expr -> String
getValName (Val name) = name
getValName _ = error "trying to get name of Expr"

app :: Parser Expr
app =  App <$> (expr' <* ws') <*> (expr')

lambda :: Parser Expr
lambda =  Lambda <$> (  lambdaLit *> ws *> (oneOf ('_':['a'..'z'])) <* ws <* char '.' <* ws) <*> (expr' <* ws )

lambdaLit :: Parser Char
lambdaLit = oneOf ['\\', 'λ', '+']

variable :: Parser Expr
variable = fmap Var $ oneOf ['a'..'z']




parens :: Parser a -> Parser a
parens p =  char '(' *> ws *>  p <* ws <*  char ')'


ws :: Parser ()
ws = void $ many $ oneOf " \n\t"

ws' :: Parser ()
ws' = void $ many1 $ oneOf " \n\t"







-- http://www.cs.cornell.edu/courses/cs6110/2014sp/Handouts/Sestoft.pdf




type Ctxt = Expr -> Expr

lambdaC :: Char -> Expr -> Expr
lambdaC x = Lambda x
app2C :: Expr -> Expr -> Expr
app2C e1  = App e1
app1C :: Expr -> Expr -> Expr
app1C e2  = flip (App) e2




data EvaluationError = VariableNotFoundException String |
                       ComputationExceedsLimitException Expr deriving Show



isRecursive :: HashMap String Expr -> String -> Expr -> Either String Bool
isRecursive hm n (Val v) = if v == n
                           then return True
                           else maybe (Left $ "Variable " ++ v ++ " not found!") (isRecursive hm n ) (HM.lookup v hm)
isRecursive _  _ (Var _) = return False
isRecursive hm n (App a b) = (||) <$> isRecursive hm n a <*> isRecursive hm n b
isRecursive hm n (Lambda _ f) = isRecursive hm n f


subst''' :: HashMap String Expr -> Char -> Expr -> Expr -> ExceptT EvaluationError (StateT Int (Writer [String])) Expr
subst''' hm c e2 (Val v) =  lookupVar v id hm (subst''' hm c e2)
subst''' hm c e2 v@(Var y) | c == y       = return e2
                           | otherwise    = return v
subst''' hm c e2 (App a b)  = do
                                e1' <- subst''' hm c e2 a
                                e2' <- subst''' hm c e2 b
                                return $ App e1' e2'
subst''' hm c e2 l@(Lambda y f) | c == y = return l
                                | otherwise = fmap (Lambda y) (subst''' hm c e2 f)
                                              -- do
                                              -- freeVNotFixed <- fmap (elem c) (freeV''' hm e2)
                                              -- if freeVNotFixed
                                              --    then
                                              --      error $ "error in subst: '" ++ [c] ++ "' in FW " ++ (show e2)
                                              --    else
                                              --      fmap (Lambda y) (subst''' hm c e2 f)





alpha''':: HashMap String Expr -> Char -> Char -> Expr -> ExceptT EvaluationError (StateT Int (Writer [String])) Expr
alpha''' hm x z (Val v) = lookupVar v id hm (alpha''' hm x z)
alpha''' hm x z (Var y) | x == y    = return $ Var z
                        | otherwise =  return  $ Var y
alpha''' hm x z (App a b) = App <$> (alpha''' hm x z a) <*> (alpha''' hm x z b)
alpha''' hm x z (Lambda y f) | x == y = return $ Lambda y f
                             | otherwise = fmap (Lambda y) (alpha''' hm x z f)


freeV''' :: HashMap String Expr -> Expr -> ExceptT EvaluationError (StateT Int (Writer [String])) [Char] -- FIXME: this method is not writer !!!
freeV''' hm (Val v) = lookupVar v id hm (freeV''' hm)
freeV''' _ (Var x) = return [x]
freeV''' hm (Lambda x t) = fmap (delete x) $ freeV''' hm t
freeV''' hm (App e1 e2) = (liftM2 union) (freeV''' hm e1) (freeV''' hm e2)


needsAlpha''' :: HashMap String Expr -> Expr -> Expr -> ExceptT EvaluationError (StateT Int (Writer [String])) Bool -- FIXME: this method is not writer !!!
needsAlpha''' hm e2 (Lambda y _) = fmap (elem y) (freeV''' hm e2)
needsAlpha''' _  _  (Val v)     = error "needsAlpha''' Val is not implemented!!"
needsAlpha''' _ _ _             = return False


fixFreeVars''' :: HashMap String Expr -> Expr -> Expr -> ExceptT EvaluationError (StateT Int (Writer [String])) (Char, Expr) -- FIXME: this method is not writer !!!
fixFreeVars''' hm e2 (Lambda x e1) = do
                                         let symbols = ['a'..'z']
                                         fv      <- freeV''' hm e2
                                         let s       = maybe (error "not enough vars!") id $ find (\z -> not $ elem z fv) symbols
                                         fmap (\y -> (s,y)) (alpha''' hm x s e1)





substc''' :: HashMap String Expr -> Ctxt -> Expr -> Expr -> ExceptT EvaluationError (StateT Int (Writer [String])) Expr
substc''' hm ctxt l@(Lambda v e1) e2 =
    do
      numSubstitutions          <- lift $ get
      if numSubstitutions > 2000
      then do
           throwE $ ComputationExceedsLimitException $ ctxt $ App l e2
      else do
           lift $ put (numSubstitutions + 1)
           needsAlpha <- needsAlpha''' hm e2 l
           if (needsAlpha)
           then do
                 (v', e1') <- fixFreeVars''' hm e2 l
                 lift $ tell $ [pprint $ ctxt $ App l e2]
                 subst''' hm v' e2 e1'
           else do
                 lift $ tell $ [pprint $ ctxt $ App l e2]
                 subst''' hm v e2 e1


lookupVar :: String -> Ctxt -> HashMap String Expr -> (Expr -> ExceptT EvaluationError (StateT Int (Writer [String])) a) -> ExceptT EvaluationError (StateT Int (Writer [String])) a
lookupVar v ctxt hm f = maybe (throwE $ VariableNotFoundException v) (f) (HM.lookup v hm)

cbn''' :: HashMap String Expr -> Ctxt -> Expr -> ExceptT EvaluationError (StateT Int (Writer [String])) Expr
cbn''' hm ctxt (Val v) = lookupVar v ctxt hm (cbn''' hm ctxt)
cbn''' _ _ v@(Var _) = return v
cbn''' _ _ l@(Lambda _ _) = return l
cbn''' hm ctxt (App e1 e2) = (cbn''' hm (ctxt . app1C e2) e1) >>= \e1' ->
                          case e1' of
                            l@(Lambda _ _) ->  (substc''' hm ctxt l e2) >>= (cbn''' hm ctxt)
                            _              -> return $ App e1' e2


-- FIXME: user bloody reader, merge contexts (for context is better to use state?)
beta''' :: HashMap String Expr -> Ctxt -> Expr -> ExceptT EvaluationError (StateT Int (Writer [String])) Expr
beta''' hm ctxt (Val v) =  lookupVar v ctxt hm (beta''' hm ctxt)
beta''' hm ctxt (App e1 e2) =   (cbn''' hm (ctxt . app1C e2) e1) >>= \e1' ->
                             case e1' of
                               l@(Lambda _ _) ->  (substc''' hm ctxt l e2) >>= (beta''' hm (ctxt))
                               _              -> do
                                                 e1'' <- beta''' hm (ctxt . app1C e2) e1'
                                                 e2'' <- beta''' hm (ctxt . app2C e1') e2
                                                 return $ App e1'' e2''
beta''' hm ctxt (Lambda v e) =  fmap (Lambda v) $ beta''' hm (ctxt . lambdaC v) e
beta''' _ _ v@(Var _) = return v


telegramMaxMessageSize:: Int
telegramMaxMessageSize = 4096

maxMessageSize:: Int
maxMessageSize = 3584

maxStackTraceSize:: Int
maxStackTraceSize = 2048

limitTrace :: String -> String
limitTrace s = if length s < maxStackTraceSize then s else "..ommitted.. ..too big for telegram.."


traceOrFail''' :: HashMap String Expr -> String -> String
traceOrFail''' hm s = either (\err -> "Can not parse input: " ++ show err) id myres
  where
    maybeExpr = parseExpression s
    myres     = fmap eval maybeExpr
    eval ex   = case resOrFail of
                 Left (VariableNotFoundException v) ->  "no such var " ++ v
                 Left (ComputationExceedsLimitException r) -> ("Computation took to long to complete. sorry..\n" ++
                                                                "Trace:\n" ++
                                                                limitTrace (printTrace xs) ++
                                                                "Last result:\n" ++
                                                                limitTrace ("==> " ++ pprint r ++ "\n")
                                                               )
                 Right (r) -> let
                                r' = printTrace xs ++ ("==> " ++ pprint r ++ "\n")
                              in case length r' of
                                  l | l < maxMessageSize -> r'
                                    | otherwise  -> let r'' = ("==> " ++ pprint r ++ "\n")
                                                    in if  length r'' < maxMessageSize then r'' else "Wow. I could't even print result. It's too big for telegram."


      where
        (resOrFail, xs) =  Writer.runWriter $ evalStateT  (runExceptT (beta''' hm id ex)) 0
        printT     ys   = concat $ fmap (\x ->  "==> " ++ x ++ "\n") ys

        printTrace ys = if length ys < 25
                          then
                             printT ys
                          else
                             let
                               header = take 5 ys
                               footer = drop (length ys - 5) ys
                               middle = "==> ... skipped " ++ (show $ length ys - 10) ++ " lines ...\n"
                             in
                               printT header ++ middle ++ (printT footer)



pprint :: Expr -> String
pprint (Var a) = [a]
pprint (App a b@(App _ _)) = pprint a ++ " " ++ "(" ++ pprint b ++ ")"
pprint (App a@(App _ _) b) =    pprint a ++ " " ++ pprint b
pprint (App l@(Lambda _ _) b) = "(" ++ pprint l ++ ")" ++ " " ++ pprint b
pprint (App a b) = "" ++ pprint a ++ " " ++ pprint b ++ ""
pprint (Lambda x b) =  "λ" ++ [x] ++ "." ++ pprint b
pprint (Val v) = v





basicVals :: HashMap String Expr
basicVals = HM.fromList [
  ("'0" , parseOrFail "λf.λx.x"),
  ("'1" , parseOrFail "λf.λx.f x"),
  ("'2" , parseOrFail "λf.λx.f (f x)"),

  ("'6" , parseOrFail "λf.λx.f (f (f (f (f (f x)))))"),

  ("Succ" , parseOrFail "λn.λf.λx.f (n f x)"),
  ("'+" , parseOrFail "λa.λb.a Succ b"),
  ("Mult" , parseOrFail "λa.λb.a (b Succ) '0"),

  ("Pair" , parseOrFail "λa.λb.λf.f a b"),
  ("Fst" , parseOrFail "λc.c (λa.λb.a)"),
  ("Snd" , parseOrFail "λc.c (λa.λb.b)"),

  ("Zeroes" , parseOrFail "Pair '0 '0"),
  ("Foo" , parseOrFail "λp.Pair (Snd p) (Succ (Snd p))"),
  ("'1Pred" , parseOrFail "λn.Fst (n Foo Zeroes)"),

  ("Pz" , parseOrFail "λx.'0"),
  ("Pf" , parseOrFail "λg.λh.h (g Succ)"),
  ("Id" , parseOrFail "λx.x"),
  ("ANotherPred" , parseOrFail "λn.n Pf Pz Id"),

  ("Pred" , parseOrFail "λn.λf.λx.n (λg.λh.h (g f)) (λ_.x) Id"),

  ("'-" , parseOrFail "λa.λb.b Pred a"),

  ("True" , parseOrFail "λx.λy.x"),
  ("False" , parseOrFail "λx.λy.y"),
  ("If" , parseOrFail "λp.λc.λa.p c a"),
  ("And" , parseOrFail "λa.λb.If a b False"),
  ("Not" , parseOrFail "λp.λa.λb.p b a"),

  ("'0?" , parseOrFail "λn.n (λ_.False) True"),
  ("'<=" , parseOrFail "λa.λb.('0? ('- a b))"),
  ("'=" , parseOrFail "λa.λb.And ('<= a b) ('<= b a)"),

  ("Nil" , parseOrFail "λc.λn.n"),
  ("IsNil" , parseOrFail "λl.l (λ_.λ_.False) True"),
  ("Cons" , parseOrFail "λh.λt.λc.λn.c h (t c n)"),
  ("Head" , parseOrFail "λl.l (λh.λt.h) False"),
  ("Append" , parseOrFail "λa.λb.a Cons b"),

  ("Nils" , parseOrFail "(Cons Nil Nil)"),
  ("Bar" , parseOrFail "λh.λp.Pair (Snd p) (Cons h (Snd p))"),
  ("Tail" , parseOrFail "λl.Fst (l Bar Nils)"),

  ("Ycomb" , parseOrFail "(λf.(λx.f (x x)) (λx.f (x x)))"),
  ("Fib'" , parseOrFail "λf.λn.If ('<= n '1) n ('+ (f (Pred n)) (f (Pred (Pred n))))"),
  ("Fib" , parseOrFail "Ycomb Fib'"),

  ("EvenoddMult" , parseOrFail "λf.Pair (λn.If ('0? n) True ((Snd f) (Pred n))) (λn.If ('0? n) False ((Fst f) (Pred n)))"),
  ("Evenodd" , parseOrFail "Ycomb EvenoddMult"),
  ("IsEven" , parseOrFail "Fst Evenodd"),
  ("IsOdd" , parseOrFail "Snd Evenodd")]
