module Main where

import BusTimes

main :: IO ()
main = getAuthInfo >>= print
