{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (IOException, catch)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Generics (Generic)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.IO (hFlush, stdout)

data Message = Message
    { msgId :: Int
    , content :: String
    }
    deriving (Show, Generic)

instance Aeson.ToJSON Message where
    toJSON (Message i c) =
        Aeson.object ["id" Aeson..= i, "content" Aeson..= c]

instance Aeson.FromJSON Message where
    parseJSON = Aeson.withObject "Message" $ \v ->
        Message
            <$> v Aeson..: "id"
            <*> v Aeson..: "content"

main :: IO ()
main = do
    sock <- socket AF_UNIX Stream 0
    connect sock (SockAddrUnix "/tmp/lark-saga.sock")

    let loop msgIdCounter = do
            putStr "Enter message content (or 'quit' to exit): "
            hFlush stdout
            input <- getLine
            if input == "quit"
                then do
                    putStrLn "Exiting."
                    close sock
                else do
                    let outgoing = Message msgIdCounter input
                        json = Aeson.encode outgoing
                    catch
                        ( do
                            sendAll sock (BL.toStrict json)
                            response <- recv sock 4096
                            case Aeson.decode (BL.fromStrict response) of
                                Just (Message i c) -> do
                                    putStrLn $ "Got reply with id: " ++ show i ++ ", content: " ++ c
                                    loop (msgIdCounter + 1)
                                Nothing -> do
                                    putStrLn "Failed to parse JSON response."
                                    close sock
                        )
                        ( \e -> do
                            putStrLn $ "Socket error: " ++ show (e :: IOException)
                            close sock
                        )

    loop 1
