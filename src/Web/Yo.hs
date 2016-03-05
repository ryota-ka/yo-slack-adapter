{-# LANGUAGE OverloadedStrings #-}

module Web.Yo (
    fromParameters
  , Yo (..)
  ) where

import Control.Lens ((^?), (^.), (^..))
import Data.Aeson ((.:), decode, Value (Object, String))
import Data.Aeson.Lens (key, nth, values)
import Data.ByteString (ByteString, isPrefixOf)
import qualified Data.ByteString.Lazy as LBS
import Data.List (find)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Network.Wreq

type Locality = String
type URL      = String
type Username = String

data Yo = JustYo     { username :: Username }
        | YoLink     { username :: Username, link :: URL }
        | YoPhoto    { username :: Username, link :: URL }
        | YoVideo    { username :: Username, link :: URL }
        | YoLocation { username :: Username, lat :: Double, lng :: Double, locality :: Maybe Locality }
        deriving (Eq, Show)

data Media = NoMedia
           | Photo
           | Video
           deriving (Show)

fromParameters :: [(String, String)] -> IO (Maybe Yo)
fromParameters params =
    traverse (flip fromParametersWithUsername params) $ lookup "username" params

fromParametersWithUsername :: Username -> [(String, String)] -> IO Yo
fromParametersWithUsername username params = do
    let link  = lookup "link"     params
        coord = lookup "location" params >>= parseCoordinate
    case (link, coord) of
         (Just link, _) -> do
             contentType <- contentTypeForURL link
             let mediaType = mediaForContentType <$> contentType
             let constructor = case mediaType of
                                    Just Photo -> YoPhoto
                                    Just Video -> YoVideo
                                    _          -> YoLink
             return $ constructor username link
         (_, Just (lat, lng)) -> do
             locality <- reverseGeocode (lat, lng)
             return $ YoLocation username lat lng locality
         _ -> return $ JustYo username
    where
        parseCoordinate :: String -> Maybe (Double, Double)
        parseCoordinate = readMaybe . ('(' :) . (++ ")") . map (\c -> if c == ';' then ',' else c)

type ContentType = ByteString

contentTypeForURL :: URL -> IO (Maybe ContentType)
contentTypeForURL link = do
    res <- head_ link
    let statusCode' = res ^. responseStatus . statusCode
        contentType = res ^? responseHeader "Content-Type"
    return $ if statusCode' == 200
                then contentType
                else Nothing

mediaForContentType :: ContentType -> Media
mediaForContentType contentType = orNoMedia
                                  . foldl (>>=) (return contentType)
                                  . map mkFilter
                                  $ [("image/", Photo), ("video/", Video)]
    where orNoMedia  = either id (const NoMedia)
          mkFilter (prefix, media) = \contentType -> if prefix `isPrefixOf` contentType
                                                        then Left media
                                                        else Right contentType

reverseGeocode :: (Double, Double) -> IO (Maybe Locality)
reverseGeocode (lat, lng) = localityForResponse <$> get url
    where
        fromString (String str) = Just str
        fromString _            = Nothing
        localityForResponse res =
            (decode (res ^. responseBody) :: Maybe Value)
                >>= return . (^.. key "results" . nth 0 . key "address_components" . values)
                >>= find (\c -> String "locality" `elem` c ^.. key "types" . values)
                >>= (^? key "long_name")
                >>= fromString
                >>= return . T.unpack
        url = concat [
            "https://maps.googleapis.com/maps/api/geocode/json?sensor=false&latlng="
          , show lat
          , ","
          , show lng
          ]
