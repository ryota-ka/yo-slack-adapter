{-# LANGUAGE OverloadedStrings #-}

module Web.YoSlackAdapter (
    slackMessageForYo
  ) where

import Data.List (intercalate)
import Data.Maybe (maybeToList)
import Web.Yo (Yo (..))
import Web.Slack.IncomingWebhook.Attachment (Attachment, defAttachment, withFallback, withImageUrl)
import Web.Slack.IncomingWebhook.Message (defMessage, Message, withAttachments, withText, withUnfurlLinks, withUsername)

slackMessageForYo :: Yo -> Message
slackMessageForYo yo =
    defMessage `withAttachments` attachments
               `withText`        text
               `withUnfurlLinks` True
               `withUsername`    "Yo"
    where
        attachments = maybeToList $ flip withFallback text <$> attachmentForYo yo
        text = textForYo yo

attachmentForYo :: Yo -> Maybe Attachment
attachmentForYo (YoPhoto _ link) = Just $ defAttachment `withImageUrl` link
attachmentForYo (YoLocation _ lat lng _) = Just $ defAttachment `withImageUrl` (staticMapUrl 16 (lat, lng))
attachmentForYo _ = Nothing

textForYo :: Yo -> String
textForYo (JustYo username) = "Yo from " ++ username
textForYo (YoLink username link) = ":link: Yo Link from " ++ username ++ "\n" ++ link
textForYo (YoPhoto username link) = ":camera: Photo from " ++ username
textForYo (YoVideo username link) = ":video_camera: Video from " ++ username ++ "\n" ++ link
textForYo (YoLocation username _ _ locality) =
     case locality of
          Just locality -> ":round_pushpin: " ++ username ++ " @ " ++ locality
          Nothing       -> ":round_pushpin: Location from " ++ username

staticMapUrl :: Int -> (Double, Double) -> String
staticMapUrl zoom (lat, lng) =
    baseUrl `withParams` [
        ("center"  , coordinate)
      , ("format"  , "png")
      , ("sensor"  , "false")
      , ("size"    , "640x640")
      , ("maptype" , "roadmap")
      , ("markers" , coordinate)
      , ("zoom"    , show zoom)
      ]
    where
        baseUrl = "https://maps.googleapis.com/maps/api/staticmap"
        coordinate = intercalate "," $ map show [lat, lng]
        withParams url = (url ++) . ('?' :) . intercalate "&" . map (uncurry eq)
        key `eq` value = mconcat [key, "=", value]
