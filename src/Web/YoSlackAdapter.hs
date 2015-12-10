{-# LANGUAGE OverloadedStrings #-}

module Web.YoSlackAdapter (
    slackMessageForYoQuery
  ) where

import Data.List (intercalate)
import Lib (getRequest)
import Network.HTTP.Conduit (responseHeaders, responseStatus)
import Network.HTTP.Types (statusCode)
import Web.Yo.Query
import Web.Slack.IncomingWebhook.Attachment (Attachment, defAttachment, withImageUrl)
import Web.Slack.IncomingWebhook.Message (defMessage, Message, withAttachments, withText, withUsername)

slackMessageForYoQuery :: Query -> IO Message
slackMessageForYoQuery q = do
    attachments <- attachmentsForAccessory $ accessory q
    text <- textForQuery q
    return $ defMessage `withAttachments` attachments `withUsername` "Yo" `withText` text

attachmentsForAccessory :: Maybe Accessory -> IO [Attachment]
attachmentsForAccessory (Just (Link link)) = do
    isImage' <- isImage link
    return $ if isImage'
                then [defAttachment `withImageUrl` link]
                else []
attachmentsForAccessory (Just (Location lat lng)) = return [defAttachment `withImageUrl` (staticMapUrl 16 (lat, lng))]
attachmentsForAccessory _ = return []

textForQuery :: Query -> IO String
textForQuery (Query username Nothing) = return $ "Yo from " ++ username
textForQuery (Query username (Just (Link link))) = do
  isImage' <- isImage link
  return $ if isImage'
              then ":camera: Photo from " ++ username ++ "\n" ++ link
              else ":link: Yo Link from " ++ username ++ "\n" ++ link
textForQuery (Query username (Just (Location lat lng))) = return $ concat [
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

isImage :: String -> IO Bool
isImage url = do
    res <- getRequest url
    let statusCode' = statusCode . responseStatus $ res
        contentType = lookup "Content-Type" . responseHeaders $ res
    return $ (statusCode' == 200 && contentType `elem` map pure imageTypes)
    where
        imageTypes = ["image/gif", "image/jpeg", "image/png"]
