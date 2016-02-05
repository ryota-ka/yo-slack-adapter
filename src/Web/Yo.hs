{-# LANGUAGE OverloadedStrings #-}

module Web.Yo (
    fromParameters
  , Yo (..)
  ) where

import Control.Lens ((^?))
import Control.Monad (mzero)
import Data.Aeson ((.:), encode, decode, FromJSON, parseJSON, Value (Object))
import Data.Aeson.Lens (key, nth)
import Data.ByteString (ByteString, isPrefixOf)
import Data.List (find)
import Lib (getRequest, headRequest)
import Network.HTTP.Conduit (responseBody, responseHeaders, responseStatus)
import Network.HTTP.Types (statusCode)
import Text.Read (readMaybe)

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
    res <- headRequest link
    let statusCode' = statusCode . responseStatus $ res
        contentType = lookup "Content-Type" . responseHeaders $ res
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

data Component = Component {
    longName  :: String
  , shortName :: String
  , types     :: [String]
  } deriving (Show)

instance FromJSON Component where
    parseJSON (Object v) = Component
                           <$> v .: "long_name"
                           <*> v .: "short_name"
                           <*> v .: "types"
    parseJSON _ = mzero

reverseGeocode :: (Double, Double) -> IO (Maybe Locality)
reverseGeocode (lat, lng) = do
    let url = endpointForCoordinate (lat, lng)
    body <- responseBody <$> getRequest url
    let components = body ^? key "results" . nth 0 . key "address_components" >>= (decode . encode) :: Maybe [Component]
    return $ components >>= find (elem "locality" . types) >>= return . longName
    where
        endpointForCoordinate (lat, lng) = concat [
            "https://maps.googleapis.com/maps/api/geocode/json?sensor=false&latlng="
          , show lat
          , ","
          , show lng
          ]
