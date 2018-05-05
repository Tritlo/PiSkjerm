{- OPTIONS_GHC -frefinement-level-hole-fits=2 -fno-max-valid-hole-fits
                -funclutter-valid-hole-fits  -}
{-# LANGUAGE OverloadedStrings #-}
module BusTimes
  ( getAuthCredentials, getOauthToken)
  where

import System.Environment
import Network.HTTP.Req

import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Char8
import Data.Default
import Debug.Trace

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

-- -- The token is the base64 encoded string "key:secret",
-- -- and then "Basic " prepended to that.
-- authToken :: String -> String -> ByteString
-- authToken key secret = pack $ "Basic " ++ (encodeToken $ key ++ ":" ++ secret)
--   where
--     -- We need to base64 encode this authorization,
--     -- so let's use the base64-bytesting package.
--     encodeToken :: String -> String
--     encodeToken token = result
--       where
--         -- First we need to get to bytestring
--         toBS :: String -> ByteString
--         -- toBs = _
--         -- pack has the right type and sounds reasonable, let's use that!
--         toBS = pack
--         -- Next we need to turn the bytestring into base64 string
--         b64ed :: ByteString
--         -- b64ed = _ $ toBs token
--         -- B64.encode sounds like what we need!
--         b64ed = B64.encode $ toBS token
--         -- And finally, we need to turn it back into a string
--         result :: String
--         -- result = _ b64ed
--         -- unpack should to the trick!
--         result = unpack b64ed

-- Now, let's try to fetch a token!
-- We know that we want a get request with some auth header, but how do we
-- get these options? Let's ask GHC:

getOauthToken :: IO ByteString
getOauthToken =
  do (key, secret) <- traceShowId <$> getAuthCredentials
     let auth = basicAuth (pack key) (pack secret)
     runReq def $ responseBody <$> req GET url NoReqBody bsResponse auth
 where url = https "api.vasttrafik.se" /: "token"

