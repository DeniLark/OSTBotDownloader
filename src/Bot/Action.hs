module Bot.Action where

import Data.Text (Text)
import qualified Data.Text as Text

data Action
  = Start
  | NoAction
  | FindGame Text
  | Link Integer Text
  | SaveGames Integer [(Text, Text)]
  | ProcessGamePage Text
  deriving
    ( Read
    , Show
    )

startMessage :: Text
startMessage =
  Text.unlines
    [ "Для того чтобы найти саудтреки к игре,"
    , "воспользуйся командой:"
    , "/find название игры"
    , "Например:"
    , "/find total war warhammer"
    , "/find fallout"
    -- , "/find космические рейнджеры" --  потому что type = Gamerip(searchGame)
    ]