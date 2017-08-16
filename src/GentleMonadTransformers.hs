{-# LANGUAGE OverloadedStrings #-}

module GentleMonadTransformers () where


import Data.Text
import qualified Data.Text.IO as T
import Data.Map as Map
import Control.Applicative


data LoginError =   InvalidEmail
                  | NoSuchUser
                  | WrongPassword deriving Show

getDomain :: Text -> Either LoginError Text
getDomain email =
  case splitOn "@" email of
    [name, domain] -> Right domain
    _              -> Left InvalidEmail

-- :set -XOverloadedStrings

printResult' :: Either LoginError Text -> IO ()
printResult' domain =
  case domain of
    Right text -> T.putStrLn (append "Domain: " text)
    Left  InvalidEmail -> T.putStrLn "ERROR: Invalid domain"


printResult'' :: Either LoginError Text -> IO ()
printResult'' = T.putStrLn . either
      (const  "ERROR: Invalid domain")
      (append "Domain: ")

getToken' :: IO (Either LoginError Text)
getToken' = do
  T.putStrLn "Enter email address"
  email <- T.getLine
  return (getDomain email)



users :: Map Text Text
users = Map.fromList [("example.com", "qwerty123"),("localhost", "password")]


userLogin' :: IO (Either LoginError Text)
userLogin' = do
  token <- getToken'

  case token of
    Right domain ->
      case Map.lookup domain users of
        Just userpw -> do
          T.putStrLn "Enter password: "
          password <- T.getLine

          if userpw == password
            then return token
            else return (Left WrongPassword)
        Nothing -> return (Left NoSuchUser)
    left -> return left

-------------------- meat -----------------

-- ExceptT :: IO (Either e a) -> ExceptT e a
-- runExceptT :: ExceptT e a -> IO (Either e a)
data ExceptT e m a = ExceptT {
  runExceptT :: m (Either e a)
}

instance Functor m => Functor (ExceptT e m) where
  fmap f  = ExceptT . fmap (fmap f) . runExceptT

instance Applicative m => Applicative (ExceptT e m) where
  pure = ExceptT . pure . Right
  f <*> x = ExceptT $ liftA2 (<*>) (runExceptT f) (runExceptT x)

instance Monad m => Monad (ExceptT e m) where
  return  = ExceptT . return . Right
  x >>= f = ExceptT $ runExceptT x >>= either (return . Left) (runExceptT . f)


getToken :: ExceptT LoginError IO Text
getToken = do
    lift $ T.putStrLn "Enter email address"
    email <- lift $ T.getLine
    liftEither (getDomain email)

liftEither :: Monad m => Either e a -> ExceptT e m a
liftEither x = ExceptT (return x)

lift :: Functor m => m a -> ExceptT e m a
lift x = ExceptT (fmap Right x)


throwE :: Monad m => e -> ExceptT e m a
throwE x = liftEither (Left x)

catchE :: Monad m => ExceptT e m a -> (e -> ExceptT e m a) -> ExceptT e m a
catchE throwing handler =
    ExceptT $ do
      result <- runExceptT throwing
      case result of
        Left failure -> runExceptT $ handler failure
        success      -> return success

wrongPasswordHandler :: LoginError -> ExceptT LoginError IO Text
wrongPasswordHandler WrongPassword = do
  lift (T.putStrLn "Wrong password, one more chance.")
  userLogin
wrongPasswordHandler err = throwE err

printError :: LoginError -> ExceptT LoginError IO a
printError err = do
  lift . T.putStrLn $ case err of
     WrongPassword -> "Wrong password."
     NoSuchUser    -> "No such user"
     InvalidEmail  -> "Invalid email"
  throwE err

loginDialogue :: ExceptT LoginError IO ()
loginDialogue = do
  let retry = userLogin `catchE` wrongPasswordHandler
  token <- retry `catchE` printError
  lift $ T.putStrLn (append "Logged in with token: " token)


userLogin :: ExceptT LoginError IO Text
userLogin = do
  token <- getToken

  userpw <- maybe (throwE NoSuchUser) return (Map.lookup token users)

  password <- lift $  T.putStrLn "Enter password: " >> T.getLine

  if userpw == password
      then return token
      else throwE WrongPassword

printResult :: Either LoginError Text -> IO ()
printResult res = T.putStrLn $ case res of
     Right token -> append "Logged in with token: " token
     Left InvalidEmail -> "Invalid email"
     Left NoSuchUser   -> "No such user"
     Left WrongPassword -> "Wrong password."
