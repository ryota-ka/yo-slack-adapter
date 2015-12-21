{-# LANGUAGE OverloadedStrings #-}

module Web.Slack.IncomingWebhook (
    sendMessage
  ) where

import Data.Aeson (encode)
import Network (withSocketsDo)
import Network.HTTP.Conduit
import Web.Slack.IncomingWebhook.Attachment (Attachment)
import Web.Slack.IncomingWebhook.Message (defMessage, Message)

sendMessage :: String -> Message -> IO ()
sendMessage url message = do
    withSocketsDo $ do
        request' <- parseUrl url
        let request = request' {
            checkStatus = \_ _ _ -> Nothing
          , method = "POST"
          , rawBody = True
          , requestBody = RequestBodyLBS . encode $ message
        }
        manager <- newManager tlsManagerSettings
        res <- httpLbs request manager
        return ()
