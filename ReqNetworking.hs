{-# LANGUAGE OverloadedStrings #-}
module ReqNetworking (
  hsGetToken, hsGetDateTime, hsGetBusTimes
) where

import BusTimes

-- Req networking
import Network.HTTP.Req
import Data.Default
import Control.Monad.IO.Class
import Data.Time
import Data.Time.Format.ISO8601
import Data.Text (Text)
import qualified Data.Text as T
import Control.Retry

import qualified Data.ByteString.Char8 as B
import qualified Data.Aeson as JSON

config :: HttpConfig
config = def {httpConfigRetryPolicy = delay }
  where
    -- We default to delaying for 5 sec and trying 10 times.
    delay :: RetryPolicy
    delay = constantDelay (5000*1000) <> limitRetries 5
hsGetToken :: IO (Maybe TokenResponse)
hsGetToken = fmap Just $ runReq config $
  do (key, secret) <- liftIO getAuthCredentials
     let auth = basicAuth (B.pack key) (B.pack secret)
         url = https "api.vasttrafik.se" /: "token"
         -- We need a body, but how? Let's ask!
         body :: ReqBodyBs
         -- body = _ ("grant_type=client_credentials" :: ByteString)
         body = ReqBodyBs "grant_type=client_credentials"
     responseBody <$> req POST url body jsonResponse auth

hsGetDateTime :: IO (Text, Text)
hsGetDateTime =
 do now <- zonedTimeToLocalTime <$> getZonedTime
    return (toDate now, toTime now)
  where toDate = T.pack . iso8601Show . localDay
        toTime = T.pack . Prelude.take 5 . iso8601Show . localTimeOfDay

hsGetBusTimes :: Token -> BusStop -> IO (Maybe BusResponse)
hsGetBusTimes token stop = runReq config $
  do (date, time) <- liftIO hsGetDateTime
     res <- responseBody <$> req GET url NoReqBody bsResponse (auth <> params date time)
     case JSON.eitherDecodeStrict res of
        Left err -> liftIO $ print err >> return Nothing
        Right v -> return v
  where url = https "api.vasttrafik.se" /: "bin" /: "rest.exe" /: "v2" /: "departureBoard"
        auth = oAuth2Bearer $ B.pack token
        params date time = "id" =: stop
                        <> "maxDeparturesPerLine" =: (2 :: Integer)
                        <> "format" =: ("json" :: String)
                        <> "timeSpan" =: (59 :: Integer)
                        <>  "date" =: date
                        <>  "time" =: time
