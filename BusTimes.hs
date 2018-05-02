{-# OPTIONS_GHC -frefinement-level-hole-fits=2 -fno-max-valid-hole-fits -funclutter-valid-hole-fits #-}
module BusTimes where

import System.Environment
import Network.HTTP.Headers

import Data.ByteString.Base64

getAuthInfo :: IO (String, String)
-- First we have to read the secrets from the environment,
-- so we add System.Environment to the import list and try
-- getSecret = _
-- one of he matches is the getEnv function!
-- Let's use that and get the key and secret
-- getAuthInfo = do secret <- _a "VASTTRAFIKSECRET"
--                  key <- _b "VASTTRAFIKKEY"
--                  return (secret, key)
-- One of the matches is getEnv, which sounds like what
-- we want. Let's try that:
getAuthInfo = do secret <- getEnv "VASTTRAFIKSECRET"
                 key <- getEnv "VASTTRAFIKKEY"
                 return (secret, key)
-- It works!

-- Now we need to construct the authentication head.
-- Let's add Network.HTTP.Headers (seems reasonable)
authHeader :: String -> Header
-- authHeader token = _
-- it suggests mkHeader (_ :: HeaderName) (_ :: String),
-- so let's try
-- authHeader token = mkHeader _ token
-- That suggests a lot, so let's look through all with
-- -fno-max-valid-hole-fits and -funclutter-valid-hole-fits
-- authHeader token = mkHeader _ token
-- HdrAuthorization is in the list, so let's use that!
authHeader token = mkHeader HdrAuthorization token

-- But we need to base64 encode our header,
