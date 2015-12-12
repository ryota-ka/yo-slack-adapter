{-# LANGUAGE OverloadedStrings #-}

module Web.YoSlackAdapter.ReverseGeocode (
    reverseGeocode
  ) where

import Control.Lens ((^?))
import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.Lens
import Data.List (find)
import Lib (getRequest)
import Network.HTTP.Conduit (responseBody)

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

endpointForCoordinate :: (Double, Double) -> String
endpointForCoordinate (lat, lng) = concat [
    "https://maps.googleapis.com/maps/api/geocode/json?sensor=false&latlng="
  , show lat
  , ","
  , show lng
  ]

reverseGeocode :: (Double, Double) -> IO (Maybe String)
reverseGeocode (lat, lng) = do
    let url = endpointForCoordinate (lat, lng)
    body <- responseBody <$> getRequest url
    let components = body ^? key "results" . nth 0 . key "address_components" >>= (decode . encode) :: Maybe [Component]
    return $ components >>= find (elem "locality" . types) >>= return . longName
