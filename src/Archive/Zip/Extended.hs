{-# LANGUAGE LambdaCase #-}

module Archive.Zip.Extended where

import Codec.Archive.Zip

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import System.Directory
import System.Directory.Extended

runArchive :: IO ()
runArchive = do
    es <- mkEntrySelector "file.txt"
    createArchive "a.zip" $ addEntry Store "hello world!!!" es

fileToArchiveIfNotEmpty :: EntrySelector -> ByteString -> ZipArchive ()
fileToArchiveIfNotEmpty es bs
    | B.null bs = pure ()
    | otherwise = createArchiveRow es bs

createArchiveRow :: EntrySelector -> ByteString -> ZipArchive ()
createArchiveRow = flip $ addEntry Deflate

toArchive :: [(EntrySelector, ByteString)] -> ZipArchive ()
toArchive = foldr ((>>) . uncurry createArchiveRow) (pure ())

createArchiveByteString :: FilePath -> ZipArchive a -> IO ByteString
createArchiveByteString filePath zipArchive = do
    newFilePath <-
        doesFileExist filePath >>= \case
            True -> pure $ changeFileName filePath
            False -> pure filePath
    _ <- createArchive newFilePath zipArchive
    bs <- B.readFile newFilePath
    removeFile newFilePath
    pure bs

createArchiveSafe :: FilePath -> ZipArchive a -> IO a
createArchiveSafe filePath zipArchive = do
    doesFileExist filePath >>= \case
        True -> createArchiveSafe (changeFileName filePath) zipArchive
        False -> createArchive filePath zipArchive
