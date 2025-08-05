{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Simple
import Network.HTTP.Types (methodPost, status200, status400)
import qualified Network.Wai as W
import Network.Wai.Handler.Warp (run)

data Payload = Payload
    { msgId :: Int
    , content :: Text
    }
    deriving (Show, Generic)

instance FromJSON Payload
instance ToJSON Payload

app :: W.Application
app req respond
    | W.requestMethod req == methodPost && W.rawPathInfo req == "/" = do
        body <- W.strictRequestBody req
        case decode body :: Maybe Payload of
            Just payload -> do
                putStrLn "Received and decoded JSON:"
                print payload

                let newJson = encode payload
                _ <- postToOtherService newJson
                putStrLn "Forwarded to external service."

                respond $ W.responseLBS status200 [("Content-Type", "application/json")] newJson
            Nothing -> do
                putStrLn "Failed to decode JSON."
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
    putStrLn "Running server on http://localhost:9999"
    run 9999 app
