module Run where

import Codec.Archive.Zip (EntrySelector, mkEntrySelector)
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.HTTP.Extended
import Parser.GameOST
import Zenacy.HTML (htmlParseEasy)

--
searchGame ::
    Text -> -- game name
    IO ()
searchGame gameName = do
    let url = addGameNameToRequest gameName

    eRes <- getPageTextFromUrl url
    case eRes of
        Left e -> print e
        Right resText -> do
            let x = pageSearchGameToGames resText
            print $ length x
            mapM_ print $ pageSearchGameToGames resText

addGameNameToRequest :: Text -> String
addGameNameToRequest gameName =
    concat
        [ "https://downloads.khinsider.com/search?search="
        , T.unpack gameName
        , "&type=album&sort=relevance&album_year=&album_category=&album_type=1"
        ]

-- https://downloads.khinsider.com/search?search=fallout&type=album&sort=relevance&album_year=&album_category=&album_type=1

collectSongPageLinks :: Text -> IO [Text]
collectSongPageLinks url = do
    eRes <- getPageTextFromUrl $ T.unpack url
    case eRes of
        Left e -> print e >> pure []
        Right resText -> pure $ processOneGamePage resText

pageSongToSongLink :: Text -> IO (Maybe Text)
pageSongToSongLink url = do
    eRes <- getPageTextFromUrl $ T.unpack url
    case eRes of
        Left e -> print e >> pure Nothing
        Right resText -> pure (processOneSongPage resText)

oneSong ::
    Int ->
    Text ->
    ([(EntrySelector, BS.ByteString)], Int) ->
    IO ([(EntrySelector, BS.ByteString)], Int)
oneSong n url (bss, i) = do
    eRes <- getPageTextFromUrl $ T.unpack url
    res <- case eRes of
        Left e -> print e >> pure bss
        Right resText -> do
            let x = processOneSongPage resText
            case x of
                Nothing -> do
                    putStrLn $ T.unpack url <> "No song found"
                    pure bss
                Just songUrl -> do
                    maybeBSES <- songDownload songUrl
                    case maybeBSES of
                        Nothing -> pure bss
                        Just bses -> pure $ bses : bss
    putStrLn $ "Completed: " <> show i <> "/" <> show n
    pure (res, i + 1)

songDownload :: Text -> IO (Maybe (EntrySelector, BS.ByteString))
songDownload songUrl = do
    eSong <- getByteStringFromUrl $ T.unpack songUrl
    case eSong of
        Left e -> print e >> pure Nothing
        Right songBytes -> do
            -- BS.writeFile fp songBytes
            es <- mkEntrySelector $ urlToFileName songUrl
            pure $ pure (es, songBytes)
