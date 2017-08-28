{-# LANGUAGE BangPatterns #-}
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
    --  , isRecursive
      , pprint
      , singleStep

) where


-- FIXME: Ycomb '6 OOM
-- FIXME: normal vars, like this_isVar and `a` <- this is also var) equalize them)
-- FIXME: single step
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
import Text.Parsec.Char (oneOf, char, digit, letter, satisfy, string, noneOf)
import Text.Parsec.Combinator (many1, chainl1, between, eof, optionMaybe,sepBy, notFollowedBy, anyToken)
import Control.Applicative ((<$>), (<**>), (<*>), (<*), (*>), (<|>), many, (<$))
import Control.Monad (void, ap, liftM2)
import Data.Char (isLetter, isDigit)
import qualified Data.Vector as V
import Data.Char(digitToInt)
import Data.List(delete, union, find)
import Debug.Trace(trace, traceShow, traceShowId)
import Control.Monad.Writer.Strict(Writer,tell)
import Control.Monad.Trans.State.Lazy
import qualified Control.Monad.Writer.Strict as Writer
--import Control.Applicative
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.Except
import Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Set(Set)
import qualified Data.Set as Set

data Variable = Variable Char


data Cmd = Help

data Expr = Var String  | App Expr Expr | Lambda String Expr  -- | const
            deriving (Show, Eq)


logLimit :: Int
logLimit = 10

data LambdaLog = LambdaLog ![String] !Int ![String] deriving (Show, Eq)

instance Monoid LambdaLog where
  mempty = LambdaLog [] 0 []
  (LambdaLog aHead aLength aTail) `mappend` (LambdaLog bHead bLength bTail) =
      LambdaLog fst5 totalLength lst5
    where
      totalLength = aLength + bLength
      (!fst5, rst5) = splitAt 5 $ aHead ++ (reverse  aTail) ++ bHead ++ (reverse bTail)
      !lst5 = take 5 $ reverse rst5--bTail ++ (reverse bHead) ++ aTail ++ (reverse aHead)

logS :: String -> LambdaLog
logS s = LambdaLog [s] 1 []

logSS :: [String] -> LambdaLog
logSS xs = LambdaLog (take 5 xs) (length xs) (reverse $ drop 5 xs)
  where
    l = length xs




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




variable :: Parser Expr
variable = Var <$> variableName

variableName :: Parser String
variableName = many1 (noneOf ".()\\λ \n\t")


variableDef :: Parser (String, Expr)
variableDef = (,) <$> (variableName <* ws <* char '=' <* ws) <*> expr''




lambda :: Parser Expr
lambda =  Lambda <$> (  lambdaLit *> ws *> variableName <* ws <* char '.' <* ws) <*> (expr' <* ws )

lambdaLit :: Parser Char
lambdaLit = oneOf ['\\', 'λ', '+']






parens :: Parser a -> Parser a
parens p =  char '(' *> ws *>  p <* ws <*  char ')'


ws :: Parser ()
ws = void $ many $ oneOf " \n\t"

ws' :: Parser ()
ws' = void $ many1 $ oneOf " \n\t"







-- http://www.cs.cornell.edu/courses/cs6110/2014sp/Handouts/Sestoft.pdf




type Ctxt = Expr -> Expr

lambdaC :: String -> Expr -> Expr
lambdaC x = Lambda x
app2C :: Expr -> Expr -> Expr
app2C e1  = App e1
app1C :: Expr -> Expr -> Expr
app1C e2  = flip (App) e2




data EvaluationError = VariableNotFoundException String |
                       ComputationExceedsLimitException Expr deriving Show



subst''' :: String -> Expr -> Expr -> ExceptT EvaluationError (StateT Int (Writer LambdaLog)) Expr
subst'''  c e2 v@(Var y) | c == y       = return e2
                           | otherwise    = return v
subst'''  c e2 (App a b)  = do
                                e1' <- subst'''  c e2 a
                                e2' <- subst'''  c e2 b
                                return $ App e1' e2'
subst'''  c e2 l@(Lambda y f) | c == y = return l
                                | otherwise = fmap (Lambda y) (subst'''  c e2 f)
                                              -- do
                                              -- freeVNotFixed <- fmap (elem c) (freeV''' hm e2)
                                              -- if freeVNotFixed
                                              --    then
                                              --      error $ "error in subst: '" ++ [c] ++ "' in FW " ++ (show e2)
                                              --    else
                                              --      fmap (Lambda y) (subst''' hm c e2 f)





alpha'''::  String -> String -> Expr ->  Expr
alpha'''  x z (Var y) | x == y    =  Var z
                        | otherwise =    Var y
alpha'''  x z (App a b) = App (alpha'''  x z a)  (alpha'''  x z b)
alpha'''  x z (Lambda y f) | x == y =   Lambda y f
                             | otherwise =  Lambda y (alpha'''  x z f)


freeV''' ::  Expr -> Set String -- FIXME: this method is not writer !!!
freeV'''  (Var x) =   Set.singleton x
freeV'''  (Lambda x t) =  (Set.delete x) $ freeV''' t
freeV'''  (App e1 e2) =  Set.union (freeV'''  e1) (freeV'''  e2)


size''' ::  Expr -> Int
size'''  (Var x) = 1
size'''  (Lambda _ t) =1 + size''' t
size'''  (App e1 e2) = size''' e1 + size''' e2


needsAlpha''' ::  Expr -> Expr -> ExceptT EvaluationError (StateT Int (Writer LambdaLog)) Bool -- FIXME: this method is not writer !!!
needsAlpha'''  e2 (Lambda y _) = return $  (elem y) (freeV''' e2)
needsAlpha'''  _ _             = return False


fixFreeVars''' ::  Expr -> Expr -> ExceptT EvaluationError (StateT Int (Writer LambdaLog)) (String, Expr) -- FIXME: this method is not writer !!!
fixFreeVars'''  e2 (Lambda x e1) = do
                                         let symbols = fmap (\i -> x ++ (show i)) [1..]
                                         let fv      = freeV'''  e2
                                         let s       = head $ filter (\z -> not $ elem z fv) symbols
                                         return (s, alpha'''  x s e1)





substc''' ::  Ctxt -> Expr -> Expr -> ExceptT EvaluationError (StateT Int (Writer LambdaLog)) Expr
substc'''  ctxt l@(Lambda v e1) e2 =
    do
      numSubstitutions          <- lift $ get
      if numSubstitutions <= 0
      then do
           throwE $ ComputationExceedsLimitException $ ctxt $ App l e2
      else do
           lift $ put (numSubstitutions - 1)
           needsAlpha <- needsAlpha'''  e2 l
           if (needsAlpha)
           then do
                 (v', e1') <- fixFreeVars'''  e2 l
                 lift $ tell $ logS $ pprint $ ctxt $ App l e2
                 subst'''  v' e2 e1'
           else do
                 lift $ tell $ logS $ pprint $ ctxt $ App l e2
                 subst'''  v e2 e1



maybeToEither :: a -> Maybe b -> Either a b
maybeToEither = flip maybe Right . Left

lookupVar :: String -> HashMap String Expr -> Either Expr Expr
lookupVar v  hm = maybeToEither (Var v) (HM.lookup v hm)

cbn''' :: HashMap String Expr -> Ctxt -> Expr -> ExceptT EvaluationError (StateT Int (Writer LambdaLog)) Expr
cbn''' hm ctxt xpr@(Var v) = either return (checkExecution ctxt xpr . cbn''' hm ctxt) (lookupVar v  hm)
cbn''' _ _ l@(Lambda _ _) = return l
cbn''' hm ctxt (App e1 e2) = (cbn''' hm (ctxt . app1C e2) e1) >>= \e1' ->
                          case e1' of
                            l@(Lambda _ _) ->  (substc'''  ctxt l e2) >>= (cbn''' hm ctxt)
                            _              -> return $ App e1' e2


-- FIXME: user bloody reader, merge contexts (for context is better to use state?)
beta''' :: HashMap String Expr -> Ctxt -> Expr -> ExceptT EvaluationError (StateT Int (Writer LambdaLog)) Expr
beta''' hm ctxt (App e1 e2) =   (cbn''' hm (ctxt . app1C e2) e1) >>= \e1' ->
                             case e1' of
                               l@(Lambda _ _) ->  (substc'''  ctxt l e2) >>= (beta''' hm (ctxt))
                               _              -> do
                                                 e1'' <- beta''' hm (ctxt . app1C e2) e1'
                                                 e2'' <- beta''' hm (ctxt . app2C e1') e2
                                                 return $ App e1'' e2''
beta''' hm ctxt (Lambda v e) =  fmap (Lambda v) $ beta''' hm (ctxt . lambdaC v) e
beta''' hm ctxt xpr@(Var v) =   either return (checkExecution ctxt xpr . beta''' hm ctxt) (lookupVar v  hm)


checkExecution :: Ctxt -> Expr -> ExceptT EvaluationError (StateT Int (Writer LambdaLog)) Expr -> ExceptT EvaluationError (StateT Int (Writer LambdaLog)) Expr
checkExecution ctxt xpr f =
                   do
                   numSubstitutions          <- lift $ get
                   if numSubstitutions <= 0
                   then do
                        throwE $ ComputationExceedsLimitException $ ctxt $ xpr
                   else
                        do
                        lift $ put (numSubstitutions - 1)
                        f


telegramMaxMessageSize:: Int
telegramMaxMessageSize = 4096

maxMessageSize:: Int
maxMessageSize = 3584

maxStackTraceSize:: Int
maxStackTraceSize = 2048

limitTrace :: String -> String
limitTrace s = if length s < maxStackTraceSize then s else "..ommitted.. ..too big for telegram.."


singleStep :: HashMap String Expr -> String -> String
singleStep hm s = either (\err -> "Can not parse input: " ++ show err) id myres
  where
    myres   = fmap eval (parseExpression s)
    eval ex   = case resOrFail of
                 Left (VariableNotFoundException v) ->  "no such var " ++ v
                 Left (ComputationExceedsLimitException r) -> limitTrace ("==> " ++ pprint r ++ "\n")

                 Right (r) -> let
                                r' =  ("==> " ++ pprint r ++ "\n")
                              in case length r' of
                                  l | l < maxMessageSize -> r'
                                    | otherwise  -> let r'' = ("==> " ++ pprint r ++ "\n")
                                                    in if  length r'' < maxMessageSize then r'' else "Wow. I could't even print result. It's too big for telegram."


      where
        (resOrFail, _) =  Writer.runWriter $ evalStateT  (runExceptT (beta''' hm id ex)) 1




traceOrFail''' :: HashMap String Expr -> String -> String
traceOrFail''' hm s = either (\err -> "Can not parse input: " ++ show err) id myres
  where
    maybeExpr = parseExpression s
    myres     = fmap eval maybeExpr
    eval ex   = case resOrFail of
                 Left (VariableNotFoundException v) ->  "no such var " ++ v
                 Left (ComputationExceedsLimitException r) -> ("Computation took to long to complete. sorry..\n" ++
                                                                "Trace:\n" ++
                                                                limitTrace(printLambdaLog ll) ++
                                                                "Last result:\n" ++
                                                                limitTrace ("==> " ++ pprint r ++ "\n")
                                                               )
                 Right (r) -> let
                                r' = (printLambdaLog ll) ++ ("==> " ++ pprint r ++ "\n")
                              in case length r' of
                                  l | l < maxMessageSize -> r'
                                    | otherwise  -> let r'' = ("==> " ++ pprint r ++ "\n")
                                                    in if  length r'' < maxMessageSize then r'' else "Wow. I could't even print result. It's too big for telegram."


      where
        (resOrFail, ll) =  Writer.runWriter $ evalStateT  (runExceptT (beta''' hm id ex)) 3000
        printT     ys   = concat $ fmap (\x ->  "==> " ++ x ++ "\n") ys

        printLambdaLog :: LambdaLog -> String
        printLambdaLog (LambdaLog logH ls logT) =
          if (ls < 10)
          then printT (logH  ++ reverse logT)
          else printT logH ++ ( "==> ... skipped " ++ (show $ ls - 10) ++ " lines ...\n" ) ++ printT (reverse logT)



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
pprint (Var a) = a
pprint (App a b@(App _ _)) = pprint a ++ " " ++ "(" ++ pprint b ++ ")"
pprint (App a@(App _ _) b) =    pprint a ++ " " ++ pprint b
pprint (App l@(Lambda _ _) b) = "(" ++ pprint l ++ ")" ++ " " ++ pprint b
pprint (App a b) = "" ++ pprint a ++ " " ++ pprint b ++ ""
pprint (Lambda x b) =  "λ" ++ x ++ "." ++ pprint b






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
