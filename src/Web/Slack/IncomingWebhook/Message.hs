{-# LANGUAGE DeriveGeneric #-}

module Web.Slack.IncomingWebhook.Message (
    defMessage
  , Message (..)
  , withAttachments
  , withChannel
  , withIconEmoji
  , withIconUrl
  , withText
  , withUsername
  ) where

import Data.Aeson (genericToJSON, toJSON, ToJSON)
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import GHC.Generics
import Web.Slack.IncomingWebhook.Attachment (Attachment)

data Message = Message {
    messageAttachments :: [Attachment]
  , messageChannel     :: Maybe String
  , messageIconEmoji   :: Maybe String
  , messageIconUrl     :: Maybe String
  , messageText        :: Maybe String
  , messageUsername    :: Maybe String
  }
  deriving (Eq, Generic, Show)

instance ToJSON Message where
    toJSON = genericToJSON $ aesonPrefix snakeCase

defMessage :: Message
defMessage = Message {
    messageAttachments = []
  , messageChannel     = Nothing
  , messageIconEmoji   = Nothing
  , messageIconUrl     = Nothing
  , messageText        = Nothing
  , messageUsername    = Nothing
  }

withAttachments :: Message -> [Attachment] -> Message
withAttachments m attachments = m { messageAttachments = attachments }

withChannel :: Message -> String -> Message
withChannel m channel = m { messageChannel = Just channel }

withIconEmoji :: Message -> String -> Message
withIconEmoji m iconEmoji = m { messageIconEmoji = Just iconEmoji }

withIconUrl :: Message -> String -> Message
withIconUrl m iconUrl = m { messageIconUrl = Just iconUrl }

withText :: Message -> String -> Message
withText m text = m { messageText = Just text }

withUsername :: Message -> String -> Message
withUsername m username = m { messageUsername = Just username }
