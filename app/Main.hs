{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import GHC.Generics (Generic)
import Network.HTTP.Simple
import Network.HTTP.Types (methodPost, status200, status400)
import qualified Network.Wai as W
import Network.Wai.Handler.Warp (run)
import System.Directory (createDirectoryIfMissing)
import System.IO (BufferMode (..), Handle, IOMode (AppendMode), hFlush, hSetBuffering, withFile)

data Payload = Payload
    { msgId :: Int
    , content :: Text
    }
    deriving (Show, Generic)

instance FromJSON Payload
instance ToJSON Payload

data LogEntry = LogEntry
    { timestamp :: Text
    , message :: Text
    }
    deriving (Show, Generic)

instance ToJSON LogEntry

logMsg :: Handle -> Text -> IO ()
logMsg h msg = do
    now <- getCurrentTime
    let ts = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" now
        entry = LogEntry ts msg
        encoded = encode entry
    BL.hPutStrLn h encoded

app :: Handle -> W.Application
app h req respond
    | W.requestMethod req == methodPost && W.rawPathInfo req == "/" = do
        body <- W.strictRequestBody req
        case decode body :: Maybe Payload of
            Just payload -> do
                logMsg h "Received and decoded JSON:"
                logMsg h (T.pack (show payload))

                let newJson = encode payload
                _ <- postToOtherService newJson
                logMsg h "Forwarded to external service."

                respond $ W.responseLBS status200 [("Content-Type", "application/json")] newJson
            Nothing -> do
                logMsg h "Failed to decode JSON."
                respond $ W.responseLBS status400 [("Content-Type", "text/plain")] "Invalid JSON"
    | otherwise = respond $ W.responseLBS status400 [("Content-Type", "text/plain")] "Bad Request"

postToOtherService :: BL.ByteString -> IO (Response BL.ByteString)
postToOtherService json = do
    initReq <- parseRequest "POST http://localhost:8888/receive"
    let req =
            setRequestBodyLBS json $
                setRequestHeader "Content-Type" ["application/json"] $
                    initReq
    httpLBS req

main :: IO ()
main = do
    createDirectoryIfMissing True "logs"

    withFile "logs/out.log" AppendMode $ \h -> do
        hSetBuffering h LineBuffering
        logMsg h "Running server on http://localhost:9999"
        hFlush h
        run 9999 (app h)
