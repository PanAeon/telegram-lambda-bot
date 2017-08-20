module Main where

import           Shlambda
import           Data.Text(Text)
import           Control.Monad.IO.Class(liftIO)
import           Debug.Trace(trace, traceShow, traceShowId)
import           Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as HM
import           Control.Concurrent.STM(TVar, newTVarIO, newTVar, atomically, modifyTVar, readTVarIO)
import           System.IO.Unsafe(unsafePerformIO, unsafeInterleaveIO)
import           System.IO
import           Control.Monad.Loops(iterateUntil)
import           Control.Monad(void)
import           GHC.IO.Encoding



type Variables = HashMap String Expr

variables :: TVar Variables
variables = unsafePerformIO $ unsafeInterleaveIO $ newTVarIO basicVals


repl :: IO ()
repl = void $ iterateUntil id $ do
           str <- getLine
           if str == ":q"
             then return True
             else do
                  if looksLikeValueDef str then
                    case regularParse valDef str of
                      Left err -> putStrLn $ "could not parse value definition: " ++  show err
                      Right (name, e) -> do
                                          vs <- readTVarIO variables
                                          case  isRecursive vs name e of
                                            Left err -> putStrLn err
                                            Right r  -> if r
                                                        then putStrLn $ "Recursive definition: " ++ name ++ " appears in rhs: " ++ pprint e
                                                        else  liftIO $ atomically $  modifyTVar variables (\vals ->
                                                                 HM.insert name e vals
                                                              )

                  else
                    do
                    vals <- readTVarIO variables
                    putStrLn $ traceOrFail''' vals str
                  return False



main :: IO ()
main = do
       setLocaleEncoding utf8
       repl
