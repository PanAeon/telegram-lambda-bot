{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module App where

-- FIXME: replace all String with TEXT !!!
--import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.Aeson.Types(fieldLabelModifier)
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger       (withStdoutLogger)
import           Network.HTTP.Client(newManager)
import           Network.HTTP.Client.TLS( tlsManagerSettings)
import           Servant
import           Servant.Client
import           System.IO
import           Shlambda( beta''', parseExpression, Expr,
                         looksLikeValueDef, regularParse, variableDef, traceOrFail''', basicVals,
                         pprint)
import           Data.Text(Text)
import           Control.Monad.IO.Class(liftIO)
import           Control.Monad(void)
import           Debug.Trace(trace, traceShow, traceShowId)
import           Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as HM
import           Control.Concurrent.STM(TVar, newTVarIO, newTVar, atomically, modifyTVar, readTVarIO)
import           System.IO.Unsafe(unsafePerformIO, unsafeInterleaveIO)
import           System.Environment
import           Network.Wai.Middleware.RequestLogger
import qualified Data.List as L
import           Control.Concurrent(forkIO, ThreadId)
import           Data.Ord
-- * api

type ItemApi =
  "new-message" :> ReqBody '[JSON] TelegramUpdate  :>  Post '[PlainText] String :<|>
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
type SessionStorage = HashMap Int ChatSession

sessionStorage :: TVar SessionStorage
sessionStorage = unsafePerformIO $ unsafeInterleaveIO $ newTVarIO HM.empty

telegramAPIKey :: String
telegramAPIKey =  unsafePerformIO $ unsafeInterleaveIO $
                   do
                   maybeKey <- lookupEnv "TelegramAPIKey"
                   maybe
                     (putStrLn "TelegramAPIKey not found! using stub!" >> return "<stub>")
                     (\key -> putStrLn ("Key" ++ key) >>  return key)
                     maybeKey


run :: IO ()
run = do
  withStdoutLogger $ \aplogger -> do
      port <-  fmap ( read  . (maybe "3000" id)) $ lookupEnv "PORT" :: IO Int
      let
          settings =
            setPort port $
            setTimeout 60 $
            setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
            setLogger aplogger defaultSettings
      runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return  $ logStdoutDev $ serve itemApi server

server :: Server ItemApi
server =
  newMessage :<|>
  getItems :<|>
  getItemById



-- FIXME: optimize free var lookup

getVals chatId ss = hm
  where
    (ChatSession hm _) = HM.lookupDefault (ChatSession basicVals ChatSettings) chatId ss

newMessage :: TelegramUpdate -> Handler String
newMessage (TelegramUpdate (Just (TelegramMessage (TelegramChat chatId) (Just msgText)))) =
     do
     if L.isPrefixOf "/" msgText then
       case msgText of
         _ | L.isPrefixOf "/run" msgText -> void $ liftIO $ handleMessage chatId ( drop 5 msgText)
           | L.isPrefixOf "/help syntax" msgText -> liftIO $ doSendMsg (SendMessage chatId $
                                                                "λx.x\n" ++
                                                                "\\x.x\n" ++
                                                                "a b\n" ++
                                                                "(a b)\n" ++
                                                                "X = <expr>\n" ++
                                                                "'0 = λf.λx.x"
                                                              )
           | L.isPrefixOf "/help" msgText -> liftIO $ doSendMsg (SendMessage chatId $
                                                         "\n" ++
                                                         "/help              -  show this useles info\n" ++
                                                         "/help syntax       -  print syntax help\n" ++
                                                         "/show vals         -  print all defined vals\n" ++
                                                         "/run <expr or def> - runs expr\n"
                                                       )
           | L.isPrefixOf "/show vals" msgText -> do
                                                    vals <- fmap (getVals chatId) (liftIO $ readTVarIO sessionStorage)
                                                    let
                                                      f (k, v) r = k ++ "  -  " ++ (pprint v) ++ "\n" ++ r -- FIXME: difference list
                                                      foo = L.sortBy (comparing fst) (HM.toList vals)
                                                      res = foldr f "" foo
                                                    liftIO $ doSendMsg (SendMessage chatId res )
           | otherwise                    -> liftIO $ doSendMsg (SendMessage chatId "unknown command")
     else
      void $ liftIO $ handleMessage chatId msgText
     return ""
newMessage _ = return "ok"



handleMessage :: Int -> String -> IO ThreadId
handleMessage chatId msgText = forkIO $
      do
        if looksLikeValueDef msgText then
            case regularParse variableDef msgText of
              Left err -> doSendMsg (SendMessage chatId $ "could not parse value definition: " ++  show err)
              Right (name, e) ->  liftIO $ atomically $  modifyTVar sessionStorage (\storage -> -- could have just used write tvalue..
                                                         let
                                                           vs =  getVals chatId storage
                                                           update _ (ChatSession hm s)  = ChatSession (HM.insert name e vs) s
                                                         in HM.insertWith update chatId (ChatSession (HM.insert name e basicVals) ChatSettings) storage
                                                       )

        else
          do
          vals <- fmap (getVals chatId) (liftIO $ readTVarIO sessionStorage)
          doSendMsg (SendMessage  chatId $ traceOrFail''' vals msgText)



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
  manager <- newManager tlsManagerSettings
  res <- runClientM (queries msg telegramAPIKey) (ClientEnv manager (BaseUrl Https "api.telegram.org" 443  ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (resp) -> do
      putStrLn $ show resp

-- * Client


data TelegramChat = TelegramChat {
    _id:: Int
  } deriving Generic


instance FromJSON TelegramChat where
  parseJSON = genericParseJSON defaultOptions {
               fieldLabelModifier = drop 1 }


data TelegramUpdate = TelegramUpdate {
  _message :: Maybe TelegramMessage
} deriving Generic

instance FromJSON TelegramUpdate where
  parseJSON = genericParseJSON defaultOptions {
               fieldLabelModifier = drop 1 }

data TelegramMessage = TelegramMessage -- actually  update
  { _chat :: TelegramChat,
    _text :: Maybe String

  } deriving Generic

instance FromJSON TelegramMessage where
  parseJSON = genericParseJSON defaultOptions {
               fieldLabelModifier = drop 1 }

data SendMessage = SendMessage
  {
      sm_chat_id :: Int
    , sm_text    :: String
    --,sm_parse_mode :: Maybe String FIXME: support Markdown
  } deriving Generic

instance ToJSON SendMessage where
    toJSON = genericToJSON defaultOptions {
                 fieldLabelModifier = drop 3 }
-- * item



data SentMessage = SentMessage {
    _ok:: Bool
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
