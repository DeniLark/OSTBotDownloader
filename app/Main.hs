module Main where

import Bot.Run
import Run (searchGame)

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"

    searchGame "fallout"

-- run
