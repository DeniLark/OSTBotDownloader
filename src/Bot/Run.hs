{-# LANGUAGE ScopedTypeVariables #-}

module Bot.Run where

-- import Bot.Model

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Servant.Client (ClientEnv (ClientEnv, makeClientRequest, manager))

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (forM_)
import Network.HTTP.Client.TLS (setGlobalManager)
import Network.HTTP.Conduit (newManager, tlsManagerSettings)
import Run (collectSongPageLinks, pageSongToSongLink)
import Servant.Client.Internal.HttpClient (mkClientEnv)
import System.Environment (getEnv)
import Telegram.Bot.API
import Telegram.Bot.API.InlineMode.InlineQueryResult
import Telegram.Bot.API.InlineMode.InputMessageContent (
    defaultInputTextMessageContent,
 )
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser (
    callbackQueryDataRead,
    command,
    parseUpdate,
    text,
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
    | FindGame Text
    | Link (Maybe SomeChatId) Text

echoBot :: BotApp Model Action
echoBot =
    BotApp
        { botInitialModel = ()
        , botAction = flip updateToAction
        , botHandler = handleAction
        , botJobs = []
        }

updateToAction :: Model -> Update -> Maybe Action
updateToAction _ update =
    let someChatId = SomeChatId <$> updateChatId update
     in parseUpdate
            ( (Link someChatId <$> command "link")
                <|> (Echo <$> text)
            )
            update

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
    Echo msg ->
        model <# do
            replyText msg
    Link maybeChatId link ->
        model <# do
            case maybeChatId of
                Nothing -> replyText "No chatId provided"
                Just someChatId -> do
                    songPages <- liftIO $ collectSongPageLinks link
                    replyText $ "Найдено: " <> Text.pack (show (length songPages))

                    forM_ songPages $ \l -> do
                        maybeSong <- liftIO $ pageSongToSongLink l
                        case maybeSong of
                            Nothing -> replyText "Ничего не найдено"
                            Just song -> do
                                _ <- liftClientM $ sendAudio $ defSendAudio someChatId $ FileUrl song
                                pure ()
    FindGame _ -> undefined

{-
    search
-}
{-
    https://downloads.khinsider.com/search?
        search=fallout      - название игры
       &type=album          - можно не менять
       &sort=relevance      - можно не менять
       &album_type=1        - можно не менять
       &album_year=2009
       &album_category=37
-}
-- https://downloads.khinsider.com/search?search=&type=album&sort=relevance&album_type=1&album_year=2009&album_category=37

-- /link https://downloads.khinsider.com/game-soundtracks/album/space-rangers-2-dominators-windows-gamerip-2004