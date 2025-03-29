module Network.HTTP.Extended where

import Control.Exception (catch)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Simple (
    HttpException,
    RequestHeaders,
    Response,
    getResponseBody,
    httpBS,
    parseRequestThrow,
    setRequestHeaders,
 )
import Network.HTTP.Types (hAccept, hAcceptLanguage, hUserAgent)

urlToFileName :: Text -> FilePath
urlToFileName = fst . T.foldr foldFunc ("", False)
  where
    foldFunc :: Char -> (FilePath, Bool) -> (FilePath, Bool)
    foldFunc '/' (fp, _) = (fp, True)
    foldFunc _ (fp, True) = (fp, True)
    foldFunc ch (fp, False) = (ch : fp, False)

getPageTextFromUrl :: String -> IO (Either HttpException Text)
getPageTextFromUrl = ((decodeUtf8 <$>) <$>) . getByteStringFromUrl

type URL = String

getByteStringFromUrl :: URL -> IO (Either HttpException ByteString)
getByteStringFromUrl = ((getResponseBody <$>) <$>) . urlToResponse

-- | don't try, if error 429
urlToResponse :: URL -> IO (Either HttpException (Response ByteString))
urlToResponse url = (pure <$> action) `catch` (pure . Left)
  where
    action :: IO (Response ByteString)
    action = do
        req' <- parseRequestThrow url
        let req = setRequestHeaders defaultHeaders req'
        httpBS req

defaultHeaders :: RequestHeaders
defaultHeaders =
    [ (hAccept, "ru-RU,ru;q=0.9,en-US;q=0.8,en;q=0.7")
    , (hAcceptLanguage, "ru-RU,ru;q=0.9,en-US;q=0.8,en;q=0.7")
    , (hUserAgent, "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36")
    ]