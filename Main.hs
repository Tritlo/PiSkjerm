module Main where

import BusTimes

main :: IO ()
main = do authInfo <- uncurry authToken <$> getAuthInfo
          token <- getToken authInfo
          print token
