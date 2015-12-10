module Lib (
    getRequest
  ) where

import Data.ByteString.Lazy (ByteString)
import Network
import Network.HTTP.Conduit

getRequest :: String -> IO (Response ByteString)
getRequest url = withSocketsDo $ do
    request' <- parseUrl url
    let request = request' { checkStatus = \_ _ _ -> Nothing }
    manager <- newManager tlsManagerSettings
    res <- httpLbs request manager
    return res
