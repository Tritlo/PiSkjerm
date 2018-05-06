{-# OPTIONS_GHC -frefinement-level-hole-fits=2 -fno-max-valid-hole-fits
                -funclutter-valid-hole-fits  -fno-max-refinement-hole-fits #-}
{-# LANGUAGE OverloadedStrings, DeriveAnyClass, DeriveGeneric, TypeSynonymInstances, DuplicateRecordFields #-}
module BusTimes where

import System.Environment
import Network.HTTP.Req
import Data.Default
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import Data.Time
import Data.Time.Format.ISO8601

import GHC.Generics (Generic)

import Data.List (nub)

import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Char8 (ByteString, pack, unpack)

getAuthCredentials :: IO (String, String)
-- First we have to read the secrets from the environment,
-- so we add System.Environment to the import list and try
-- getSecret = _
-- one of he matches is the getEnv function!
-- Let's use that and get the key and secret
-- getAuthCredentials = do key <- _b "VASTTRAFIKKEY"
--                  secret <- _a "VASTTRAFIKSECRET"
--                  return (key, secret)
--
-- One of the matches is getEnv, which sounds like what
-- we want. Let's try that:
getAuthCredentials = do key <- getEnv "VASTTRAFIKKEY"
                        secret <- getEnv "VASTTRAFIKSECRET"
                        return (key, secret)
-- It works!

-- The token is the base64 encoded string "key:secret",
authToken :: String -> String -> String
authToken key secret = encodeToken $ key ++ ":" ++ secret
  where
    -- We need to base64 encode this authorization,
    -- so let's use the base64-bytesting package.
    encodeToken :: String -> String
    encodeToken token = result
      where
        -- First we need to get to bytestring
        toBS :: String -> ByteString
        -- toBs = _
        -- pack has the right type and sounds reasonable, let's use that!
        toBS = pack
        -- Next we need to turn the bytestring into base64 string
        b64ed :: ByteString
        -- b64ed = _ $ toBs token
        -- B64.encode sounds like what we need!
        b64ed = B64.encode $ toBS token
        -- And finally, we need to turn it back into a string
        result :: String
        -- result = _ b64ed
        -- unpack should to the trick!
        result = unpack b64ed

-- Now, let's try to fetch a token!
-- We know that we want a get request with some auth header, but how do we
-- get these options? Let's ask GHC:

data TokenResponse = TR { scope :: String
                        , token_type :: String
                        , expires_in:: Int
                        , access_token :: String
                        } deriving (Show, Generic, FromJSON)


          

getToken :: IO TokenResponse
getToken = runReq def $ do (key, secret) <- liftIO $ getAuthCredentials
                           let auth = basicAuth (pack key) (pack secret)
                               url = https "api.vasttrafik.se" /: "token"
                               -- We need a body, but how? Let's ask!
                               body :: ReqBodyBs
                               -- body = _ ("grant_type=client_credentials" :: ByteString)
                               body = ReqBodyBs "grant_type=client_credentials"
                           responseBody <$> req POST url body jsonResponse auth


data BusResponse = BR { departureBoard :: DepartureBoard} deriving Show

instance FromJSON BusResponse where
  parseJSON = withObject "BusResponse" $ \v -> BR <$> v .: "DepartureBoard"

data DepartureBoard = DB { departure :: Maybe [Departure] } deriving Show

instance FromJSON DepartureBoard where
  parseJSON = withObject "DepartureBoard" $ \v -> DB <$> v .: "Departure"


data Departure = DP { direction :: String
                    , track :: String
                    , sname :: String
                    , rtTime :: Maybe String
                    , rtDate :: Maybe String
                    , time :: String
                    , date :: String
                    } deriving (Show, Generic, FromJSON)

type Token = String
type BusStop = Int

getDateTime :: IO (String, String)
getDateTime = do now <- zonedTimeToLocalTime <$> getZonedTime
                 return (toDate now, toTime now)
  where toDate = iso8601Show . localDay 
        toTime = Prelude.take 5 . iso8601Show . localTimeOfDay

getBusTimes :: BusStop -> Token -> IO BusResponse
getBusTimes stop token = runReq def $
  do (date, time) <- liftIO getDateTime
     responseBody <$> req GET url NoReqBody jsonResponse (auth <> params date time)

  where url = https "api.vasttrafik.se" /: "bin" /: "rest.exe" /: "v2" /: "departureBoard"
        auth = oAuth2Bearer $ pack token
        params date time = "id" =: stop
                           <> "maxDeparturesPerLine" =: (2 :: Integer)
                           <> "format" =: ("json" :: String)
                           <> "timeSpan" =: (59 :: Integer)
                           <>  "date" =: date
                           <>  "time" =: time

data BusLine = BL { name :: String
                  , track :: String
                  , departures :: [String] } deriving (Show, Eq)

getLines :: BusResponse -> [BusLine]
getLines (BR (DB Nothing)) = []
getLines (BR (DB (Just dep))) = concatMap getLine names
  where 
    names = nub $ map sname dep
    getLine name = map toLine tracks
      where 
        dtrack = track :: Departure -> String
        tracks = nub $ map dtrack $ filter ((== name) . sname) dep
        -- filter fusion, yay!
        getTrack tr = filter ((== name) . sname)
                        . filter ((== tr) . dtrack )
        concreteTime d = case rtTime d of Just t -> t; _ -> time d
        toLine tr = BL name tr $ map concreteTime $ getTrack tr dep
