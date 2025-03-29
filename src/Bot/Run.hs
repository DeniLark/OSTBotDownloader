{-# LANGUAGE ScopedTypeVariables #-}

module Bot.Run where

-- import Bot.Model

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Servant.Client (ClientEnv (ClientEnv, makeClientRequest, manager))

import Network.HTTP.Client.TLS (setGlobalManager)
import Network.HTTP.Conduit (newManager, tlsManagerSettings)
import Servant.Client.Internal.HttpClient (mkClientEnv)
import System.Environment (getEnv)
import Telegram.Bot.API
import Telegram.Bot.API.InlineMode.InlineQueryResult
import Telegram.Bot.API.InlineMode.InputMessageContent (
    defaultInputTextMessageContent,
 )
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser (
    updateMessageText,
 )

run :: IO ()
run = do
    token <- Token . Text.pack <$> getEnv "BOT_OST_DOWNLODER"
    env <- defaultTelegramClientEnv token
    putStrLn "Running bot"
    startBot_ echoBot env

type Model = ()

data Action
    = Echo Text
    | DownloadFile (Maybe ChatId)

echoBot :: BotApp Model Action
echoBot =
    BotApp
        { botInitialModel = ()
        , botAction = updateToAction
        , botHandler = handleAction
        , botJobs = []
        }

updateToAction :: Update -> Model -> Maybe Action
updateToAction update _ =
    case updateMessageText update of
        Just text -> Just (Echo text) >> Just (DownloadFile $ updateChatId update)
        Nothing -> Nothing

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
    Echo msg ->
        model <# do
            replyText msg
    -- pure msg -- or replyText msg
    DownloadFile mChatId ->
        model <# do
            let df = DocumentFile "file.txt" "Hello world!"
                mSendDocReq = (`toSendDocument` df) . SomeChatId <$> mChatId
            case mSendDocReq of
                Nothing -> pure ()
                Just sdr -> do
                    _ <- liftClientM $ sendDocument sdr
                    pure ()
            pure ()
