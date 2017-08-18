{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module App where

--import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.Aeson.Types(fieldLabelModifier)
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.HTTP.Client (newManager, defaultManagerSettings)
import           Servant
import           Servant.Client
import           System.IO
import           Lambda(beta'', beta''', parseExpression, traceOrFail, Expr)
import           Data.Text(Text)
import           Control.Monad.IO.Class(liftIO)
import           Debug.Trace(trace, traceShow, traceShowId)
import           Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as HM
import           Control.Concurrent.STM(TVar, newTVarIO, newTVar, atomically, modifyTVar)
import           System.IO.Unsafe(unsafePerformIO, unsafeInterleaveIO)
-- * api

type ItemApi =
  "new-message" :> ReqBody '[JSON] TelegramMessage  :>  Post '[PlainText] String :<|>
  "item" :> Get '[JSON] [Item] :<|>
  "item" :> Capture "itemId" Integer :> Get '[JSON] Item

itemApi :: Proxy ItemApi
itemApi = Proxy

-- * app
-- last command, history
-- TODO: Seq[String] history
-- FIXME: last modified time
-- TODO: storage limit?
data ChatSettings = ChatSettings
data ChatSession = ChatSession (HashMap String Expr) ChatSettings
type SessionStorage = HashMap String ChatSession

sessionStorage :: TVar SessionStorage
sessionStorage = unsafePerformIO $ unsafeInterleaveIO $ newTVarIO HM.empty

run :: IO ()
run = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve itemApi server

server :: Server ItemApi
server =
  newMessage :<|>
  getItems :<|>
  getItemById

newMessage :: TelegramMessage -> Handler String
newMessage msg = do
                 session <- liftIO $ atomically $ modifyTVar sessionStorage id
                 liftIO $ doSendMsg (SendMessage chatId errOrRes)
                 return ""
        where
          chatId   = _id . _chat $ msg
          msgText = _text msg
          errOrRes = traceOrFail msgText


getItems :: Handler [Item]
getItems = return [exampleItem]

getItemById :: Integer -> Handler Item
getItemById = \ case
  0 -> return exampleItem
  _ -> throwError err404 {  errBody = "Sorry dear user." }

exampleItem :: Item
exampleItem = Item 0 "example item"

-- * Message request

{-
// If we've gotten this far, it means that we have received a message containing the word "marco".
  // Respond by hitting the telegram bot API and responding to the approprite chat_id with the word "Polo!!"
  // Remember to use your own API toked instead of the one below  "https://api.telegram.org/bot<your_api_token>/sendMessage"
  axios.post('https://api.telegram.org/bot270485614:AAHfiqksKZ8WmR2zSjiQ7_v4TMAKdiHm9T0/sendMessage', {
    chat_id: message.chat.id,
    text: 'Polo!!'
  })

-}

type TelegramAPI =  ReqBody '[JSON] SendMessage  :> Capture "api_token" String :> "sendMessage" :> Post '[JSON] SentMessage

telegramAPI :: Proxy TelegramAPI
telegramAPI = Proxy

sendMessage :: SendMessage -> [Char] -> ClientM SentMessage
sendMessage  = client telegramAPI

queries :: SendMessage -> String -> ClientM SentMessage
queries msg token = sendMessage msg ("bot" ++ token)

-- http://www.sohamkamani.com/blog/2016/09/21/making-a-telegram-bot/

doSendMsg :: SendMessage -> IO ()
doSendMsg  msg = do
  manager <- newManager defaultManagerSettings
  res <- runClientM (queries msg "<token>") (ClientEnv manager (BaseUrl Http "localhost" 8081 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (resp) -> do
      putStrLn $ show resp

-- * Client


data TelegramChat = TelegramChat {
    _id:: String
  } deriving Generic


instance FromJSON TelegramChat where
  parseJSON = genericParseJSON defaultOptions {
               fieldLabelModifier = drop 1 }

data TelegramMessage = TelegramMessage
  { _chat :: TelegramChat
  , _text :: String
  } deriving Generic

instance FromJSON TelegramMessage where
  parseJSON = genericParseJSON defaultOptions {
               fieldLabelModifier = drop 1 }

data SendMessage = SendMessage
  {
    sm_chat_id :: String,
    sm_text    :: String
  } deriving Generic

instance ToJSON SendMessage where
    toJSON = genericToJSON defaultOptions {
                 fieldLabelModifier = drop 3 }
-- * item



data SentMessage = SentMessage {
    _random_id:: Integer
  } deriving (Generic, Show)

instance FromJSON SentMessage where
      parseJSON = genericParseJSON defaultOptions {
                   fieldLabelModifier = drop 1 }

data Item
  = Item {
    itemId :: Integer,
    itemText :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Item
instance FromJSON Item

data a + b = Foo a b

type X = Int + Bool
