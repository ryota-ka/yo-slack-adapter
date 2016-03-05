{-# LANGUAGE OverloadedStrings #-}

module Web.Slack.IncomingWebhook (
    sendMessage
  ) where

import Control.Monad (void)
import Data.Aeson (encode, toJSON)
import Network (withSocketsDo)
import Network.Wreq
import Web.Slack.IncomingWebhook.Attachment (Attachment)
import Web.Slack.IncomingWebhook.Message (defMessage, Message)

sendMessage :: String -> Message -> IO ()
sendMessage url message = void $ post url (toJSON message)
