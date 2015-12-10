{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (toJSON)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Maybe (fromMaybe)
import GHC.Word (Word8)
import Network.HTTP.Types (status400)
import Network.Wai (rawQueryString)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Web.Scotty (get, html, params, request, scotty, status)
import Web.Slack.IncomingWebhook (sendMessage)
import Web.Yo.Query (fromParameters, username)
import Web.YoSlackAdapter (slackMessageForYoQuery)

parseRawQuery :: ByteString -> [(String, String)]
parseRawQuery = map (unpackBoth . splitIntoKeyVal) . BS.split 38 . BS.tail
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
            params' <- params
            case (lookup "username" params') of
                 Nothing -> status status400 >> html "Bad request"
                 Just _  -> do
                     query <- fromParameters . parseRawQuery . rawQueryString <$> request
                     message <- liftIO $ slackMessageForYoQuery query
                     liftIO . forkIO $ sendMessage slackWebhookUrl message
                     html "Yo"
