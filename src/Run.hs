module Run (run) where

import Archive.Zip.Extended (
    createArchiveSafe,
    toArchive,
 )
import Codec.Archive.Zip (EntrySelector, mkEntrySelector)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Extended
import Parser.GameOST
import System.Directory (createDirectoryIfMissing, setCurrentDirectory)

directory :: FilePath
directory = "result"

run :: IO ()
run = do
    createDirectoryIfMissing True directory
    setCurrentDirectory directory

    eRes <- getPageTextFromUrl url1

    case eRes of
        Left e -> print e
        Right resText -> do
            let result = take 3 $ processOneGamePage resText
                lengthResult = length result
            (dataSongs, _) <- foldrM (oneSong lengthResult) ([], 1) result
            putStrLn "The archive is prepared"
            createArchiveSafe "result.zip" $ toArchive dataSongs
            putStrLn "Done!!!"

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

url1 :: URL
url1 = "https://downloads.khinsider.com/game-soundtracks/album/space-rangers-2-dominators-windows-gamerip-2004"
