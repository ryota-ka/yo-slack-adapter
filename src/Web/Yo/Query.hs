module Web.Yo.Query (
    Accessory (..)
  , fromParameters
  , Query (..)
  ) where

import Data.ByteString (ByteString)
import Text.Read (readMaybe)

data Accessory =
    Link String
  | Location { lat :: Double, lng :: Double }
  deriving (Eq, Show)

data Query = Query {
    username  :: String
  , accessory :: Maybe Accessory
  }
  deriving (Eq, Show)

emptyQuery :: Query
emptyQuery = Query {
    username  = ""
  , accessory = Nothing
  }

withAccessory :: Query -> Accessory -> Query
withAccessory query accessory = query { accessory = Just accessory }

withUsername :: Query -> String -> Query
withUsername query username = query { username = username }

withLink :: Query -> String -> Query
withLink query = withAccessory query . Link

withLocation :: Query -> (Double, Double) -> Query
withLocation query (lat, lng) = withAccessory query $ Location { lat = lat, lng = lng }

fromParameters :: [(String, String)] -> Query
fromParameters = foldl (\query (key, value) -> (with key) query value) emptyQuery
    where
        with key = case key of
            "username" -> withUsername
            "link"     -> withLink
            "location" -> \q location -> case parseLocation location of
                Just (lat, lng) -> withLocation q (lat, lng)
                Nothing         -> q
            _          -> const
        parseLocation = \location -> readMaybe $ mconcat ["(", replaceSemicolonWithComma location, ")"]
        replaceSemicolonWithComma = map (\x -> if x == ';' then ',' else x)
