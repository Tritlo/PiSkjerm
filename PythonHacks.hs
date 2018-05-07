{-# LANGUAGE OverloadedStrings  #-}

module PythonHacks (
 urlRequest, pyGetToken, pyGetBusTimes, pyGetDateTime
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as E

import qualified Data.Text as T
import Data.Text hiding (map, null)

import Prelude hiding (hPutStr, hPutStrLn, hGetLine, putStrLn)
import qualified Data.Aeson as JSON

import Control.Monad.Reader

import BusTimes
import InkyPhat

-- The Nice request libraries segfault on the pi,
-- so we'll just use the already available python interpreter
data AuthHeader = Basic String | Bearer String

instance Show AuthHeader where
  show (Basic s) = show ("Basic " <> s)
  show (Bearer s) = show ("Bearer " <> s)


urlRequest :: Text -> [(Text,Text)] ->
              AuthHeader -> ByteString -> InkyIO Text
urlRequest url params auth body
 = readString readCmd
 where reqCmd = "request.Request(" <> intercalate "," args <> ")"
       readCmd = "request.urlopen(" <> reqCmd <> ").read().decode('utf8')"
       args = [ (pack $ show urlArg)
              , "headers={'Authorization':" <> (pack $ show auth) <> "}"]
              <> (if B.null body then mempty else ["data=b'" <> E.decodeUtf8 body <> "'"])
       urlArg = if null params then url
                else (url <> "?"<> ( intercalate "&" $ map paramToUrl params))
       paramToUrl (name,arg) = name <> "=" <> arg


-- | We use these to get the information via Python,
-- since the network libraries segfault on the pi
-- unless the TZ variable is set to a non-file base timezone.
-- Use these if you must.
pyGetToken :: InkyIO (Maybe TokenResponse)
pyGetToken = do (key, secret) <- lift getAuthCredentials
                let auth = authToken key secret
                    url = "https://api.vasttrafik.se/token"
                    body = "grant_type=client_credentials"
                JSON.decodeStrict . E.encodeUtf8 <$> urlRequest url [] (Basic auth) body

pyGetBusTimes :: Token -> BusStop -> InkyIO (Maybe BusResponse)
pyGetBusTimes token stop =
   do (date, time) <- pyGetDateTime
      res <- E.encodeUtf8 <$> urlRequest url (params date time) (Bearer token)  ""
      case JSON.eitherDecodeStrict res of
        Left err -> lift $ print err >> return Nothing
        Right v -> return v
  where url = "https://api.vasttrafik.se/bin/rest.exe/v2/departureBoard"
        auth = Bearer token
        params date time = [("id", pack $ show stop)
                           , ("maxDeparturesPerLine", pack $ show 2)
                           , ("format","json")
                           , ("timeSpan", pack $ show 59)
                           , ("date", date)
                           , ("time", time)]


pyGetDateTime :: InkyIO (Text, Text)
pyGetDateTime = do d <- readString "datetime.now().strftime('%Y-%m-%d')"
                   t <- readString "datetime.now().strftime('%H:%M')"
                   return (d,t)
