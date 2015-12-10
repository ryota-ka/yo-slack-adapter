{-# LANGUAGE DeriveGeneric #-}

module Web.Slack.IncomingWebhook.Attachment (
    Attachment
  , defAttachment
  , Field (..)
  , withAuthorName
  , withAuthorIcon
  , withAuthorLink
  , withColor
  , withFallback
  , withField
  , withImageUrl
  , withPretext
  , withTitle
  , withTitleLink
  , withThumbUrl
  ) where

import Data.Aeson (genericToJSON, toJSON, ToJSON)
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import GHC.Generics

data Attachment = Attachment {
    attachmentAuthorName :: Maybe String
  , attachmentAuthorIcon :: Maybe String
  , attachmentAuthorLink :: Maybe String
  , attachmentColor      :: Maybe String
  , attachmentFallback   :: Maybe String
  , attachmentFields     :: [Field]
  , attachmentImageUrl   :: Maybe String
  , attachmentPretext    :: Maybe String
  , attachmentTitle      :: Maybe String
  , attachmentTitleLink  :: Maybe String
  , attachmentThumbUrl   :: Maybe String
  }
  deriving (Eq, Generic, Show)

instance ToJSON Attachment where
    toJSON = genericToJSON $ aesonPrefix snakeCase

data Field = Field {
    fieldTitle :: Maybe String
  , fieldValue :: Maybe String
  , fieldShort :: Maybe Bool
  }
  deriving (Eq, Generic, Show)

instance ToJSON Field where
    toJSON = genericToJSON $ aesonPrefix snakeCase

defAttachment :: Attachment
defAttachment = Attachment {
    attachmentAuthorName = Nothing
  , attachmentAuthorIcon = Nothing
  , attachmentAuthorLink = Nothing
  , attachmentColor      = Nothing
  , attachmentFallback   = Nothing
  , attachmentFields     = []
  , attachmentImageUrl   = Nothing
  , attachmentPretext    = Nothing
  , attachmentTitle      = Nothing
  , attachmentTitleLink  = Nothing
  , attachmentThumbUrl   = Nothing
  }

withAuthorName :: Attachment -> String -> Attachment
withAuthorName a name = a { attachmentAuthorName = Just name }

withAuthorIcon :: Attachment -> String -> Attachment
withAuthorIcon a icon = a { attachmentAuthorIcon = Just icon }

withAuthorLink :: Attachment -> String -> Attachment
withAuthorLink a link = a { attachmentAuthorLink = Just link }

withColor :: Attachment -> String -> Attachment
withColor a color = a { attachmentColor = Just color }

withFallback :: Attachment -> String -> Attachment
withFallback a fallback = a { attachmentFallback = Just fallback }

withField :: Attachment -> Field -> Attachment
withField a field = a { attachmentFields = field : attachmentFields a }

withImageUrl :: Attachment -> String -> Attachment
withImageUrl a imageUrl = a { attachmentImageUrl = Just imageUrl }

withPretext :: Attachment -> String -> Attachment
withPretext a pretext = a { attachmentPretext = Just pretext }

withTitle :: Attachment -> String -> Attachment
withTitle a title = a { attachmentTitle = Just title }

withTitleLink :: Attachment -> String -> Attachment
withTitleLink a titleLink = a { attachmentTitleLink = Just titleLink }

withThumbUrl :: Attachment -> String -> Attachment
withThumbUrl a thumbUrl = a { attachmentThumbUrl = Just thumbUrl }
