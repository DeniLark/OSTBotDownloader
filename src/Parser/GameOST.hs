{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Parser.GameOST where

import Data.Maybe (mapMaybe)
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

--                                 name, link
pageSearchGameToGames :: Text -> [(Text, Text)]
pageSearchGameToGames text =
  let body = getBodyPage $ htmlParseEasy text
      table = bodySerchGameToTable body
      trs' = concatMap tableToTrs table
      trs = if null trs' then [] else drop 1 trs'
   in mapMaybe (trToGame . leaveOnlyTd) trs

leaveOnlyTd :: HTMLNode -> HTMLNode
leaveOnlyTd node =
  let oldContent = htmlElemContent node
   in htmlNodeContentSet (filter (htmlElemHasName "td") oldContent) node

trToGame :: HTMLNode -> Maybe (Text, Text)
trToGame = helper . htmlElemContent
 where
  helper :: [HTMLNode] -> Maybe (Text, Text)
  helper (_ : nodeGameName : _) = tdGameNameToGameName nodeGameName
  helper _ = Nothing

--                                         name, link
tdGameNameToGameName :: HTMLNode -> Maybe (Text, Text)
tdGameNameToGameName = foldr foldFunc Nothing . htmlElemContent
 where
  foldFunc :: HTMLNode -> Maybe (Text, Text) -> Maybe (Text, Text)
  foldFunc node acc
    | htmlElemHasName "a" node =
        let gameName = htmlRenderNodes $ htmlElemContent node
            gameLink = htmlElemGetAttr "href" node
         in (gameName,) . addBaseUrl baseUrl <$> gameLink
    | otherwise = foldr foldFunc acc $ htmlElemContent node

tableToTrs :: HTMLNode -> [HTMLNode]
tableToTrs = foldr foldFunc [] . htmlElemContent
 where
  foldFunc :: HTMLNode -> [HTMLNode] -> [HTMLNode]
  foldFunc node acc
    | htmlElemHasName "tr" node = node : acc
    | otherwise = foldr foldFunc acc $ htmlElemContent node

bodySerchGameToTable :: [HTMLNode] -> Maybe HTMLNode
bodySerchGameToTable = foldr foldFunc Nothing
 where
  foldFunc :: HTMLNode -> Maybe HTMLNode -> Maybe HTMLNode
  foldFunc node acc
    | htmlElemClassesContains "albumList" node = Just node
    | otherwise = foldr foldFunc acc $ htmlElemContent node

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