module Bot.Model where

import Data.Map (Map)

import qualified Data.Map as Map
import Data.Text (Text)

--             name, link
-- type Model = [(Text, Text)]

data Model = Model
    { modelGames :: Map Integer ([(Text, Text)])
    }

initialModel :: Model
initialModel = Model Map.empty

insertGamesInChatID :: Integer -> [(Text, Text)] -> Model -> Model
insertGamesInChatID chatID games = Model . Map.insert chatID games . modelGames

lookupGamesFromChatID :: Integer -> Model -> Maybe [(Text, Text)]
lookupGamesFromChatID chatID = Map.lookup chatID . modelGames

lookupGamePageFromChatID :: Integer -> Text -> Model -> Maybe Text
lookupGamePageFromChatID chatID game =
    (lookup game =<<)
        . lookupGamesFromChatID chatID