{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Maybe (fromMaybe)
import GHC.Word (Word8)
import Network.HTTP.Types (status400)
import Network.HTTP.Types.URI (urlDecode)
import Network.Wai (rawQueryString)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Web.Scotty (get, html, params, request, scotty, status)
import Web.Slack.IncomingWebhook (sendMessage)
import Web.Yo (fromParameters, Yo (..))
import Web.YoSlackAdapter (slackMessageForYo)

parseRawQuery :: ByteString -> [(String, String)]
parseRawQuery "" = []
parseRawQuery q  = map (unpackBoth . splitIntoKeyVal) . BS.split 38 . BS.tail $ q
    where
        splitIntoKeyVal = (\(k, v) -> (k, BS.tail v)) . BS.break (== 61)
        unpackBoth = \(x, y) -> (unpack x, unpack y)

main :: IO ()
main = do
    envSlackWebhookUrl <- lookupEnv "SLACK_WEBHOOK_URL"
    let !slackWebhookUrl = case envSlackWebhookUrl of
                                Just u  -> u
                                Nothing -> error "SLACK_WEBHOOK_URL is not set"

    port <- fromMaybe 8080 . (>>= readMaybe) <$> lookupEnv "PORT"

    scotty port $ do
        get "/" $ do
            params <- parseRawQuery . urlDecode False . rawQueryString <$> request
            yo <- liftIO $ fromParameters params
            case yo of
                 Nothing -> status status400 >> html "Bad Request"
                 Just yo -> do
                     let message = slackMessageForYo yo
                     liftIO . forkIO $ sendMessage slackWebhookUrl message
                     html "Yo"
