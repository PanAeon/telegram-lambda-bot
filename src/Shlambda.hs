-- {-# LANGUAGE BangPatterns #-}
module Shlambda(
        Expr(..)
      , parseExpression
      , regularParse
      , parseOrFail
      , variable
      , variableDef
      , beta'''
      , looksLikeValueDef
      , traceOrFail'''
      , basicVals
      , logLimit
      , evaluate
      , telegramMaxMessageSize
      , pprint
      , singleStep

) where

-- Changelog:
-- FIXED: Ycomb '6 OOM ! done
-- FIXED: normal vars, like this_isVar and `a` <- this is also var) equalize them)
-- FIXED: single step
-- FIXED: check message size for telegram

-- Buildpack :: https://github.com/mfine/heroku-buildpack-stack




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

-- TELEGRAM API: https://core.telegram.org/bots/api#sendmessage
import Text.Parsec (ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.Prim (parse)
import Text.Parsec.Char (oneOf, char, noneOf) -- satisfy
import Text.Parsec.Combinator (many1, eof,sepBy)
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>), many)
import Control.Monad (void)
--import qualified Data.Vector as V
--import Debug.Trace(trace, traceShow, traceShowId)
import Control.Monad.Writer.Strict(Writer,tell)
import Control.Monad.Trans.State.Strict
import qualified Control.Monad.Writer.Strict as Writer
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.Except
import Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Set(Set)
import qualified Data.Set as Set


type EvaluationState = (Int, Int)

type LambdaMonad a = ExceptT EvaluationError (StateT EvaluationState (Writer LambdaLog)) a


data Expr = Var String  | App Expr Expr | Lambda String Expr  -- | const
            deriving (Show, Eq)


type Ctxt = Expr -> Expr

data EvaluationError = VariableLookupExceedsLimitException Expr |
                       ComputationExceedsLimitException Expr deriving Show

logLimit :: Int
logLimit = 10

data LambdaLog = LambdaLog ![String] !Int ![String] deriving (Show, Eq)

instance Monoid LambdaLog where
  mempty = LambdaLog [] 0 []
  (LambdaLog aHead aLength aTail) `mappend` (LambdaLog bHead bLength bTail) =
      LambdaLog fst5 totalLength lst5
    where
      totalLength = aLength + bLength
      (fst5, rst5) = splitAt 5 $ aHead ++ reverse  aTail ++ bHead ++ reverse bTail
      lst5 = take 5 $ reverse rst5--bTail ++ (reverse bHead) ++ aTail ++ (reverse aHead)

logS :: ShowS -> LambdaLog
logS s = LambdaLog [s ""] 1 []

-- logSS :: [String] -> LambdaLog
-- logSS xs = LambdaLog (take 5 xs) (length xs) (reverse $ drop 5 xs)
--   where
--     l = length xs




parseOrFail :: String -> Expr
parseOrFail s =  either (error . show)  id (regularParse expr'' s)

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

parseExpression :: String -> Either ParseError Expr
parseExpression  = regularParse expr''

looksLikeValueDef :: String -> Bool
looksLikeValueDef s = either (const False) (const True) (regularParse (variableName <* ws' <* char '=' <* ws') s)

expr'' :: Parser Expr
expr'' = expr' <* ws <* eof

expr' :: Parser Expr
expr' =  apply

apply :: Parser Expr
apply = varOrApp <$> (expr <* ws) <*> (expr `sepBy` ws)
   where
     varOrApp e1 [] =  e1
     varOrApp e1 xs =  foldl1 App $ e1:xs

expr :: Parser Expr
expr =
            lambda
        <|> variable
        <|> parens expr'

variable :: Parser Expr
variable = Var <$> variableName

variableName :: Parser String
variableName = many1 (noneOf ".()\\λ \n\t")

variableDef :: Parser (String, Expr)
variableDef = (,) <$> (variableName <* ws <* char '=' <* ws) <*> expr''

lambda :: Parser Expr
lambda =  Lambda <$> (  lambdaLit *> ws *> variableName <* ws <* char '.' <* ws) <*> (expr' <* ws )

lambdaLit :: Parser Char
lambdaLit = oneOf ['\\', 'λ']

parens :: Parser a -> Parser a
parens p =  char '(' *> ws *>  p <* ws <*  char ')'

ws :: Parser ()
ws = void $ many $ oneOf " \n\t"

ws' :: Parser ()
ws' = void $ many1 $ oneOf " \n\t"




-- http://www.cs.cornell.edu/courses/cs6110/2014sp/Handouts/Sestoft.pdf




lambdaC :: String -> Expr -> Expr
lambdaC = Lambda
app2C :: Expr -> Expr -> Expr
app2C  = App
app1C :: Expr -> Expr -> Expr
app1C  = flip App








subst''' :: String -> Expr -> Expr -> LambdaMonad Expr
subst'''  c e2 v@(Var y) | c == y       = return e2
                           | otherwise    = return v
subst'''  c e2 (App a b)  = do
                                e1' <- subst'''  c e2 a
                                e2' <- subst'''  c e2 b
                                return $ App e1' e2'
subst'''  c e2 l@(Lambda y f) | c == y = return l
                                | otherwise = fmap (Lambda y) (subst'''  c e2 f)


alpha'''::  String -> String -> Expr ->  Expr
alpha'''  x z (Var y) | x == y    =  Var z
                        | otherwise =    Var y
alpha'''  x z (App a b) = App (alpha'''  x z a)  (alpha'''  x z b)
alpha'''  x z (Lambda y f) | x == y =   Lambda y f
                             | otherwise =  Lambda y (alpha'''  x z f)


freeV''' ::  Expr -> Set String
freeV'''  (Var x) =   Set.singleton x
freeV'''  (Lambda x t) =  Set.delete x $ freeV''' t
freeV'''  (App e1 e2) =  Set.union (freeV'''  e1) (freeV'''  e2)


needsAlpha''' ::  Expr -> Expr ->  Bool
needsAlpha'''  e2 (Lambda y _) =  y `elem` freeV''' e2
needsAlpha'''  _ _             =  False


fixFreeVars''' ::  Expr -> Expr ->  (String, Expr)
fixFreeVars'''  e2 (Lambda x e1) = let
                                       symbols = fmap (\i -> x ++ show (i::Int)) [1..]
                                       fv      = freeV'''  e2
                                       s       = head $ filter (`notElem`  fv) symbols
                                   in  (s, alpha'''  x s e1)

fixFreeVars''' _ _ = error "wrong call to fix free vars"

substc''' ::  Ctxt -> Expr -> Expr -> LambdaMonad Expr
substc'''  ctxt l@(Lambda v e1) e2 =
    checkExecution ctxt l $
           if needsAlpha'''  e2 l
           then do
                 let (v', e1') = fixFreeVars'''  e2 l
                 lift $ tell $ logS $ pprint $ ctxt $ App l e2
                 subst'''  v' e2 e1'
           else do
                 lift $ tell $ logS $ pprint $ ctxt $ App l e2
                 subst'''  v e2 e1
substc''' _ _ _ = error "Wrong substitution call!"


maybeToEither :: a -> Maybe b -> Either a b
maybeToEither = flip maybe Right . Left

lookupVar :: String -> HashMap String Expr -> Either Expr Expr
lookupVar v  hm = maybeToEither (Var v) (HM.lookup v hm)

cbn''' :: HashMap String Expr -> Ctxt -> Expr -> LambdaMonad Expr
cbn''' hm ctxt xpr@(Var v) = either return (checkLookups ctxt xpr . cbn''' hm ctxt) (lookupVar v  hm)
cbn''' _ _ l@(Lambda _ _) = return l
cbn''' hm ctxt (App e1 e2) = cbn''' hm (ctxt . app1C e2) e1 >>= \e1' ->
                          case e1' of
                            l@(Lambda _ _) ->  substc'''  ctxt l e2 >>= cbn''' hm ctxt
                            _              -> return $ App e1' e2


-- FIXME: user bloody reader, merge contexts (for context is better to use state?)
beta''' :: HashMap String Expr -> Ctxt -> Expr -> LambdaMonad Expr
beta''' hm ctxt (App e1 e2) =   cbn''' hm (ctxt . app1C e2) e1 >>= \e1' ->
                             case e1' of
                               l@(Lambda _ _) ->  substc'''  ctxt l e2 >>= beta''' hm ctxt
                               _              -> do
                                                 e1'' <- beta''' hm (ctxt . app1C e2) e1'
                                                 e2'' <- beta''' hm (ctxt . app2C e1') e2
                                                 return $ App e1'' e2''
beta''' hm ctxt (Lambda v e) =  Lambda v <$> beta''' hm (ctxt . lambdaC v) e
beta''' hm ctxt xpr@(Var v) =   either return (checkLookups ctxt xpr . beta''' hm ctxt) (lookupVar v  hm)


checkLookups :: Ctxt -> Expr -> LambdaMonad Expr -> LambdaMonad Expr
checkLookups ctxt xpr f =
                   do
                   (numSubstitutions, numLookups)          <- lift get
                   if numLookups <= 0
                   then
                     throwE $ VariableLookupExceedsLimitException $ ctxt xpr
                   else
                        lift ( put (numSubstitutions, numLookups - 1)) >> f


checkExecution :: Ctxt -> Expr -> LambdaMonad Expr -> LambdaMonad Expr
checkExecution ctxt xpr f =
                   do
                   (numSubstitutions, _)          <- lift get
                   if numSubstitutions <= 0
                   then
                        throwE $ ComputationExceedsLimitException $ ctxt xpr
                   else
                        lift ( put (numSubstitutions - 1, 50)) >> f



telegramMaxMessageSize:: Int
telegramMaxMessageSize = 4096

maxMessageSize:: Int
maxMessageSize = 3584

maxStackTraceSize:: Int
maxStackTraceSize = 2048

limitTrace :: String -> String
limitTrace s = if length s < maxStackTraceSize then s else "..ommitted.. ..too big for telegram.."


trimLongMessage :: Int -> String -> String
trimLongMessage limit str = if length str < limit
                            then str
                            else take maxMessageSize str ++ " ... <trimmed|"

singleStep :: HashMap String Expr -> String -> String
singleStep hm s = either (\err -> "Can not parse input: " ++ show err) id myres
  where
    myres   = fmap eval (parseExpression s)
    eval ex   = case resOrFail of
                 Left (ComputationExceedsLimitException r) -> limitTrace ("==> " ++ pprint r "" ++ "\n")
                 Left (VariableLookupExceedsLimitException r) -> "variable lookup exceeds limit: " ++ limitTrace ("==> " ++ pprint r "" ++ "\n")
                 Right r -> let
                                r' =  ("==> " ++ pprint r "" ++ "\n")
                              in case length r' of
                                  l | l < maxMessageSize -> r'
                                    | otherwise  -> let r'' = ("==> " ++ pprint r ""++ "\n")
                                                    in if  length r'' < maxMessageSize then r'' else "Wow. I could't even print result. It's too big for telegram."


      where
        (resOrFail, _) =  Writer.runWriter $ evalStateT  (runExceptT (beta''' hm id ex)) (1, 50)




evaluate :: HashMap String Expr -> String -> String
evaluate hm s = either (\err -> "Can not parse input: " ++ show err) id myres
  where
    maybeExpr = parseExpression s
    myres     = fmap eval maybeExpr
    eval ex   = case resOrFail of
                 Left (ComputationExceedsLimitException r) -> "Computation took to long to complete. sorry..\n" ++
                                                                "Trace:\n" ++
                                                                trimLongMessage maxStackTraceSize (printLambdaLog ll) ++
                                                                "Last result:\n" ++
                                                                trimLongMessage maxStackTraceSize ("==> " ++ pprint r "" ++ "\n")

                 Left (VariableLookupExceedsLimitException r) -> "Variable lookup exceeds limit. sorry..\n" ++
                                                                "Trace:\n" ++
                                                                trimLongMessage maxStackTraceSize (printLambdaLog ll) ++
                                                                "Last result:\n" ++
                                                                trimLongMessage maxStackTraceSize ("==> " ++ pprint r "" ++ "\n")

                 Right r -> trimLongMessage maxMessageSize $ "==> " ++ pprint r "" ++ "\n"


      where
        (resOrFail, ll) =  Writer.runWriter $ evalStateT  (runExceptT (beta''' hm id ex)) (10000, 50)


traceOrFail''' :: HashMap String Expr -> String -> String
traceOrFail''' hm s = either (\err -> "Can not parse input: " ++ show err) id myres
  where
    maybeExpr = parseExpression s
    myres     = fmap eval maybeExpr
    eval ex   = case resOrFail of
                 Left (ComputationExceedsLimitException r) -> "Computation took to long to complete. sorry..\n" ++
                                                                "Trace:\n" ++
                                                                trimLongMessage maxStackTraceSize (printLambdaLog ll) ++
                                                                "Last result:\n" ++
                                                                trimLongMessage maxStackTraceSize ("==> " ++ pprint r "" ++ "\n")

                 Left (VariableLookupExceedsLimitException r) -> "Variable lookup exceeds limit. sorry..\n" ++
                                                                "Trace:\n" ++
                                                                trimLongMessage maxStackTraceSize (printLambdaLog ll) ++
                                                                "Last result:\n" ++
                                                                trimLongMessage maxStackTraceSize  ("==> " ++ pprint r "" ++ "\n")

                 Right r ->  trimLongMessage maxStackTraceSize (printLambdaLog ll) ++
                                     trimLongMessage maxStackTraceSize ("==> " ++ pprint r "" ++ "\n")



      where
        (resOrFail, ll) =  Writer.runWriter $ evalStateT  (runExceptT (beta''' hm id ex)) (10000, 50)


printLambdaLog :: LambdaLog -> String
printLambdaLog (LambdaLog logH ls logT) =
    if ls <= 10
    then printT (logH  ++ reverse logT)
    else printT logH ++ ( "==> ... skipped " ++ show (ls - 10) ++ " lines ...\n" ) ++ printT (reverse logT)

printT :: [String] -> String
printT        = concatMap (\x ->  "==> " ++ x ++ "\n")

pprint :: Expr -> ShowS
pprint (Var a) = showString a
pprint (App a b@(App _ _)) = pprint a . showString " " . showString "(" . pprint b  . showString ")"
pprint (App a@(App _ _) b) =    pprint a . showString " " . pprint b
pprint (App l@(Lambda _ _) b) = showString "(" . pprint l . showString ")" . showString " " . pprint b
pprint (App a b) = showString "" . pprint a . showString " " . pprint b . showString ""
pprint (Lambda x b) =  showString "λ" . showString x .showString  "." . pprint b






basicVals :: HashMap String Expr
basicVals = HM.fromList [
  ("0" , parseOrFail "λf.λx.x"),
  ("1" , parseOrFail "λf.λx.f x"),
  ("2" , parseOrFail "λf.λx.f (f x)"),
  ("3" , parseOrFail "λf.λx.f (f (f x))"),
  ("4" , parseOrFail "λf.λx.f (f (f (f x)))"),
  ("5" , parseOrFail "λf.λx.f (f (f (f (f x))))"),
  ("6" , parseOrFail "λf.λx.f (f (f (f (f (f x)))))"),
  ("7" , parseOrFail "λf.λx.f (f (f (f (f (f (f x))))))"),
  ("8" , parseOrFail "λf.λx.f (f (f (f (f (f (f (f x)))))))"),
  ("9" , parseOrFail "λf.λx.f (f (f (f (f (f (f (f (f x))))))))"),

  ("succ" , parseOrFail "λn.λf.λx.f (n f x)"),
  ("+" , parseOrFail "λa.λb.a succ b"),
  ("*" , parseOrFail "λa.λb.a (b succ) 0"),

  ("pair" , parseOrFail "λa.λb.λf.f a b"),
  ("fst" , parseOrFail "λc.c (λa.λb.a)"),
  ("snd" , parseOrFail "λc.c (λa.λb.b)"),

  ("0s" , parseOrFail "pair 0 0"),
  ("foo" , parseOrFail "λp.pair (snd p) (succ (snd p))"),
  ("1?" , parseOrFail "λn.fst (n foo 0s)"),

  ("pz" , parseOrFail "λx.0"),
  ("pf" , parseOrFail "λg.λh.h (g succ)"),
  ("id" , parseOrFail "λx.x"),
  ("const", parseOrFail "λx.λy.y"),
  ("ANotherPred" , parseOrFail "λn.n pf pz id"),

  ("pred" , parseOrFail "λn.λf.λx.n (λg.λh.h (g f)) (λ_.x) id"),

  ("-" , parseOrFail "λa.λb.b pred a"),

  ("true" , parseOrFail "λx.λy.x"),
  ("false" , parseOrFail "λx.λy.y"),
  ("if" , parseOrFail "λp.λc.λa.p c a"),
  ("&&" , parseOrFail "λa.λb.if a b false"),
  ("||" , parseOrFail "λa.λb.if a true b"),
  ("!" , parseOrFail "λp.λa.λb.p b a"),

  ("0?" , parseOrFail "λn.n (λ_.false) true"),
  ("<=" , parseOrFail "λa.λb.(0? (- a b))"),
  ("=" , parseOrFail "λa.λb.&& (<= a b) (<= b a)"),

  ("nil" , parseOrFail "λc.λn.n"),
  ("nil?" , parseOrFail "λl.l (λ_.λ_.false) true"),
  ("::" , parseOrFail "λh.λt.λc.λn.c h (t c n)"),
  ("head" , parseOrFail "λl.l (λh.λt.h) false"),
  ("++" , parseOrFail "λa.λb.a cons b"),

  ("nils" , parseOrFail "(cons Nil Nil)"),
  ("bar" , parseOrFail "λh.λp.pair (snd p) (cons h (snd p))"),
  ("tail" , parseOrFail "λl.fst (l bar nils)"),

  ("Y" , parseOrFail "(λf.(λx.f (x x)) (λx.f (x x)))"),
  ("fib'" , parseOrFail "λf.λn.if (<= n 1) n ( + ( f (pred n)) (f (pred (pred n))))"),
  ("fib" , parseOrFail "Y fib'"),

  ("evenodd'" , parseOrFail "λf.pair (λn.if (0? n) true ((snd f) (pred n))) (λn.if (0? n) false ((fst f) (pred n)))"),
  ("evenodd" , parseOrFail "Y evenodd'"),
  ("even?" , parseOrFail "fst evenodd"),
  ("odd?" , parseOrFail "snd evenodd")]
