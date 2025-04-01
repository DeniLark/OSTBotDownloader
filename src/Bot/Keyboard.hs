module Bot.Keyboard where

import Bot.Action
import Data.Text (Text)
import Telegram.Bot.API
import Telegram.Bot.Simple.InlineKeyboard (actionButton)

--                         name, link
-- gamesToInlineKeyboard :: [(Text, Text)] -> InlineKeyboardMarkup
-- gamesToInlineKeyboard =
--     InlineKeyboardMarkup
--         . foldr foldFunc []
--   where
--     foldFunc :: (Text, Text) -> [[InlineKeyboardButton]] -> [[InlineKeyboardButton]]
--     foldFunc (nGame, lGame) acc = [actionButton nGame (LinkBtn lGame)] : acc

createKeyboardButton :: Text -> KeyboardButton
createKeyboardButton text =
    KeyboardButton
        { keyboardButtonText = text
        , keyboardButtonRequestUsers = Nothing
        , keyboardButtonRequestChat = Nothing
        , keyboardButtonRequestContact = Nothing
        , keyboardButtonRequestLocation = Nothing
        , keyboardButtonRequestPoll = Nothing
        , keyboardButtonWebApp = Nothing
        }

gameNamesToKeyboard :: [Text] -> ReplyKeyboardMarkup
gameNamesToKeyboard gameNames =
    ReplyKeyboardMarkup
        { replyKeyboardMarkupKeyboard =
            map (pure . createKeyboardButton) gameNames
        , replyKeyboardMarkupIsPersistent = Just False
        , replyKeyboardMarkupResizeKeyboard = Just True
        , replyKeyboardMarkupOneTimeKeyboard = Just True
        , replyKeyboardMarkupSelective = Just True
        , replyKeyboardMarkupInputFieldSelector = Nothing
        }

defaultKeyboard :: ReplyKeyboardMarkup
defaultKeyboard =
    ReplyKeyboardMarkup
        { replyKeyboardMarkupKeyboard = []
        , replyKeyboardMarkupIsPersistent = Just False
        , replyKeyboardMarkupResizeKeyboard = Just False
        , replyKeyboardMarkupOneTimeKeyboard = Just False
        , replyKeyboardMarkupSelective = Just True
        , replyKeyboardMarkupInputFieldSelector = Nothing
        }
