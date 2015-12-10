module Web.YoSlackAdapter (
    slackMessageForYoQuery
  ) where

import Data.List (intercalate)
import Web.Yo.Query
import Web.Slack.IncomingWebhook.Attachment (Attachment, defAttachment, withImageUrl)
import Web.Slack.IncomingWebhook.Message (defMessage, Message, withAttachments, withText, withUsername)

slackMessageForYoQuery :: Query -> Message
slackMessageForYoQuery q = defMessage `withAttachments` attachments `withUsername` "Yo" `withText` text
    where
        attachments = attachmentsForAccessory $ accessory q
        text = textForQuery q

attachmentsForAccessory :: Maybe Accessory -> [Attachment]
attachmentsForAccessory (Just (Location lat lng)) = [defAttachment `withImageUrl` (staticMapUrl 16 (lat, lng))]
attachmentsForAccessory _ = []

textForQuery :: Query -> String
textForQuery (Query username Nothing) = "Yo from " ++ username
textForQuery (Query username (Just (Link link))) = concat [
    ":link: Yo Link from "
  , username
  , "\n"
  , link
  ]
textForQuery (Query username (Just (Location lat lng))) = concat [
    ":round_pushpin: "
  , username
  , " @ ("
  , show lat
  , ", "
  , show lng
  , ")"
  ]

staticMapUrl :: Int -> (Double, Double) -> String
staticMapUrl zoom (lat, lng) =
    foldl (++) baseUrl $ concatParams [
        ("center"  , coordinate)
      , ("format"  , "png")
      , ("sensor"  , "false")
      , ("size"    , "640x640")
      , ("maptype" , "roadmap")
      , ("markers" , coordinate)
      , ("zoom"    , show zoom)
      ]
    where
        baseUrl = "https://maps.googleapis.com/maps/api/staticmap?"
        concatParams = map (\(k, v) -> concat ["&", k, "=", v])
        coordinate = intercalate "," $ map show [lat, lng]
