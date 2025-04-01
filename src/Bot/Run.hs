{-# LANGUAGE ScopedTypeVariables #-}

module Bot.Run where

-- import Bot.Model

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Servant.Client (ClientEnv (ClientEnv, makeClientRequest, manager))

import Bot.Action (Action (..), startMessage)
import Bot.Keyboard
import Bot.Model
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (forM_)
import Network.HTTP.Client.TLS (setGlobalManager)
import Network.HTTP.Conduit (newManager, tlsManagerSettings)
import Run (collectSongPageLinks, pageSongToSongLink, searchGame)
import Servant.Client.Internal.HttpClient (mkClientEnv)
import System.Environment (getEnv)
import Telegram.Bot.API hiding (editMessageReplyMarkup)
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

echoBot :: BotApp Model Action
echoBot =
    BotApp
        { botInitialModel = initialModel
        , botAction = flip updateToAction
        , botHandler = handleAction
        , botJobs = []
        }

chatIdToInteger :: ChatId -> Integer
chatIdToInteger (ChatId n) = n

updateToAction :: Model -> Update -> Maybe Action
updateToAction _ update =
    let _ = chatIdToInteger <$> updateChatId update
     in parseUpdate
            ( -- (Link someChatId <$> command "link")
              (Start <$ command "start")
                <|> (FindGame <$> command "find")
                <|> ProcessGamePage <$> text
            )
            update

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
    Link chatID link ->
        model <# do
            songPages <- liftIO $ collectSongPageLinks link
            replyText $ "Найдено: " <> Text.pack (show (length songPages))

            forM_ songPages $ \l -> do
                maybeSong <- liftIO $ pageSongToSongLink l
                case maybeSong of
                    Nothing -> replyText "Ничего не найдено"
                    Just song -> do
                        _ <-
                            liftClientM $
                                sendAudio $
                                    defSendAudio (SomeChatId $ ChatId chatID) $
                                        FileUrl song
                        pure ()
    ProcessGamePage game ->
        model <# do
            maybeChatId <- (chatIdToInteger <$>) <$> currentChatId

            case maybeChatId of
                Nothing -> do
                    liftIO $ putStrLn "Without chat id"
                    pure NoAction
                Just chatID ->
                    case lookupGamePageFromChatID chatID game model of
                        Nothing -> pure NoAction
                        Just url -> pure $ Link chatID url
    FindGame gameName ->
        model <# do
            -- _ <- liftIO $ print gameName
            games <- liftIO $ searchGame gameName
            -- _ <- liftIO $ mapM_ print games
            reply
                (toReplyMessage $ "Поиск по запросу: " <> gameName <> " завершен")
                    { replyMessageReplyMarkup =
                        Just $
                            SomeReplyKeyboardMarkup $
                                gameNamesToKeyboard $
                                    map fst games
                    }

            maybeChatId <- (chatIdToInteger <$>) <$> currentChatId

            case maybeChatId of
                Nothing -> do
                    liftIO $ putStrLn "Without chat id"
                    pure NoAction
                Just chatID -> pure $ SaveGames chatID games
    NoAction -> pure model
    SaveGames chatID games -> pure $ insertGamesInChatID chatID games model
    Start ->
        model <# do
            replyText startMessage

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