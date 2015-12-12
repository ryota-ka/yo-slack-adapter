{-# LANGUAGE OverloadedStrings #-}

module Web.Yo (
    fromParameters
  , Yo (..)
  ) where

import Control.Lens ((^?))
import Control.Monad (mzero)
import Data.Aeson ((.:), encode, decode, FromJSON, parseJSON, Value (Object))
import Data.Aeson.Lens (key, nth)
import Data.List (find)
import Lib (getRequest)
import Network.HTTP.Conduit (responseBody, responseHeaders, responseStatus)
import Network.HTTP.Types (statusCode)
import Text.Read (readMaybe)

type Locality = String
type Username = String
type URL      = String

data Yo = JustYo     { username :: Username }
        | YoLink     { username :: Username, link :: URL }
        | YoPhoto    { username :: Username, link :: URL }
        | YoLocation { username :: Username, lat :: Double, lng :: Double, locality :: Maybe Locality }
        deriving (Eq, Show)

fromParameters :: [(String, String)] -> IO (Maybe Yo)
fromParameters params = case lookup "username" params of
                             Nothing       -> return Nothing
                             Just username -> do
                                 yo <- Just <$> fromParametersWithUsername username params
                                 return yo

fromParametersWithUsername :: Username -> [(String, String)] -> IO Yo
fromParametersWithUsername username params = do
    case (lookup "link" params, lookup "location" params >>= parseCoordinate) of
         (Just link, _) -> do
             isImage' <- isImage link
             let constructor = if isImage' then YoPhoto else YoLink
             return $ constructor username link
         (_, Just (lat, lng)) -> do
             locality <- reverseGeocode (lat, lng)
             return $ YoLocation username lat lng locality
         _ -> return $ JustYo username
    where
        parseCoordinate :: String -> Maybe (Double, Double)
        parseCoordinate = readMaybe . ('(' :) . (++ ")") . map (\c -> if c == ';' then ',' else c)

isImage :: URL -> IO Bool
isImage link = do
    res <- getRequest link
    let statusCode' = statusCode . responseStatus $ res
        contentType = lookup "Content-Type" . responseHeaders $ res
    return $ (statusCode' == 200 && contentType `elem` map pure imageTypes)
    where
        imageTypes = ["image/gif", "image/jpeg", "image/png"]

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

endpointForCoordinate :: (Double, Double) -> URL
endpointForCoordinate (lat, lng) = concat [
    "https://maps.googleapis.com/maps/api/geocode/json?sensor=false&latlng="
  , show lat
  , ","
  , show lng
  ]

reverseGeocode :: (Double, Double) -> IO (Maybe Locality)
reverseGeocode (lat, lng) = do
    let url = endpointForCoordinate (lat, lng)
    body <- responseBody <$> getRequest url
    let components = body ^? key "results" . nth 0 . key "address_components" >>= (decode . encode) :: Maybe [Component]
    return $ components >>= find (elem "locality" . types) >>= return . longName
