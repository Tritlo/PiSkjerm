{-# LANGUAGE OverloadedStrings, DeriveAnyClass, DeriveGeneric,
             TypeSynonymInstances, DuplicateRecordFields #-}
module BusTimes where

import System.Environment
import Data.Aeson
import Data.Aeson.Types

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

data TokenResponse = TR { scope :: String
                        , token_type :: String
                        , expires_in:: Int
                        , access_token :: String
                        } deriving (Show, Generic, FromJSON)

data BusResponse = BR { departureBoard :: DepartureBoard} deriving Show

instance FromJSON BusResponse where
  parseJSON = withObject "BusResponse" $ \v -> BR <$> v .: "DepartureBoard"

data DepartureBoard = DB { departure :: [Departure] } deriving Show

instance FromJSON DepartureBoard where
  parseJSON = withObject "DepartureBoard" $ \v -> DB <$> v .:? "Departure" .!= []


data Departure = DP { direction :: String
                    , track :: String
                    , sname :: String
                    , rtTime :: Maybe String
                    , rtDate :: Maybe String
                    , time :: String
                    , date :: String
                    } deriving (Show, Generic, FromJSON)

type Token = String
type BusStop = Integer

data BusLine = BL { name :: String
                  , track :: String
                  , departures :: [String] } deriving (Show, Eq)

getLines :: BusResponse -> [BusLine]
getLines (BR (DB dep)) = concatMap getLine names
  where names = nub $ map sname dep
        getLine name = map toLine tracks
          where dtrack = track :: Departure -> String
                tracks = nub $ map dtrack $ filter ((== name) . sname) dep
                -- filter fusion, yay!
                getTrack tr = filter ((== name) . sname)
                                . filter ((== tr) . dtrack )
                concreteTime d = case rtTime d of Just t -> t; _ -> time d
                toLine tr = BL name tr $ map concreteTime $ getTrack tr dep
