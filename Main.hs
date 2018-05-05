module Main where

import BusTimes

main :: IO ()
main = getOauthToken >>= print
