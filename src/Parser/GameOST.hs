{-# LANGUAGE ScopedTypeVariables #-}

module Parser.GameOST where

import Data.Text (Text)
import qualified Data.Text as T
import Zenacy.HTML

baseUrl :: Text
baseUrl = "https://downloads.khinsider.com"

processOneGamePage :: Text -> [Text]
processOneGamePage text =
    let body = getBodyPage $ htmlParseEasy text
        songList = bodyToSongListContent body
     in songListToSongLinks songList

processOneSongPage :: Text -> Maybe Text
processOneSongPage text =
    let body = getBodyPage $ htmlParseEasy text
     in bodyToAudio body

bodyToAudio :: [HTMLNode] -> Maybe Text
bodyToAudio = foldr foldFunc Nothing
  where
    foldFunc :: HTMLNode -> Maybe Text -> Maybe Text
    foldFunc _ res@(Just _) = res
    foldFunc node _
        | htmlElemHasID "audio" node = htmlElemGetAttr "src" node
        | otherwise = foldr foldFunc Nothing $ htmlElemContent node

bodyToSongListContent :: [HTMLNode] -> [HTMLNode]
bodyToSongListContent = foldr foldFunc []
  where
    foldFunc :: HTMLNode -> [HTMLNode] -> [HTMLNode]
    foldFunc node []
        | htmlElemHasID "songlist" node = htmlElemContent node
        | otherwise = foldr foldFunc [] $ htmlElemContent node
    foldFunc _ acc = acc

songListToSongLinks :: [HTMLNode] -> [Text]
songListToSongLinks = foldr foldFunc []
  where
    foldFunc :: HTMLNode -> [Text] -> [Text]
    foldFunc node acc
        | htmlElemClassesContains "playlistDownloadSong" node =
            nodeWithLinkToLink node : acc
        | otherwise = foldr foldFunc acc $ htmlElemContent node

nodeWithLinkToLink :: HTMLNode -> Text
nodeWithLinkToLink = foldr foldFunc "" . htmlElemContent
  where
    foldFunc :: HTMLNode -> Text -> Text
    foldFunc node ""
        | htmlElemHasName "a" node =
            maybe "" (addBaseUrl baseUrl) (htmlElemGetAttr "href" node)
    foldFunc _ link = link

addBaseUrl :: Text -> Text -> Text
addBaseUrl bUrl url =
    case T.uncons url of
        Nothing -> ""
        Just ('/', restUrl) -> case T.unsnoc bUrl of
            Nothing -> url
            Just (_, '/') -> bUrl <> restUrl
            Just (_, _) -> bUrl <> url
        Just _ -> case T.unsnoc bUrl of
            Nothing -> url
            Just (_, '/') -> bUrl <> url
            Just (_, _) -> bUrl <> T.cons '/' url

getBodyPage :: HTMLNode -> [HTMLNode]
getBodyPage = maybe [] htmlNodeContent . htmlDocBody