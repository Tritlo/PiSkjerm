module Main where

import BusTimes

löviksVägen :: BusStop
löviksVägen = 9021014004663000
hovåsNedre :: BusStop
hovåsNedre = 9021014003235000
main :: IO ()
main = do token <- access_token <$>  getToken
          lvg <- getBusTimes löviksVägen token
          hvs <- getBusTimes hovåsNedre token
          print lvg
          print hvs
