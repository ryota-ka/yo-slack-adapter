{-# LANGUAGE OverloadedStrings #-}

module Lib (
    getRequest
  , headRequest
  ) where

import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy.Internal as LBS
import Network
import Network.HTTP.Conduit

getRequest  = simpleRequest "GET"
headRequest = simpleRequest "HEAD"

type Method = BS.ByteString
type URL = String

simpleRequest :: Method -> URL -> IO (Response LBS.ByteString)
simpleRequest method url = withSocketsDo $ do
    request' <- parseUrl url
    let request = request' { method = method, checkStatus = \_ _ _ -> Nothing }
    manager <- newManager tlsManagerSettings
    res <- httpLbs request manager
    return res
