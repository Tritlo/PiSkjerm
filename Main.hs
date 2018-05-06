{-# LANGUAGE TypeApplications #-}
module Main where

import BusTimes
import InkyPhat

import Data.Maybe
import Data.Time

löviksVägen :: BusStop
löviksVägen = 9021014004663000
hovåsNedre :: BusStop
hovåsNedre = 9021014003235000

setup :: InkyIO ()
setup = do setRotation 180
           vt <- image "vt" "vt.png"
           return ()

fontheight :: Int
fontheight = 12
linespace :: Int
linespace = 2

printBusTimes :: [String] -> InkyIO ()
printBusTimes msgs =
  do (inkyH, inkyW) <- dimensions
     let (x,y) = (2, (inkyH `div` 2 - fontheight*(length msgs) `div` 2))
         pr (m,i) = text pos m Nothing Nothing  
          where pos = (x, y + i*(fontheight + linespace))
     mapM_ pr $ zip msgs [0.. ]

  where pos = fromMaybe

displayNames :: BusLine -> String
displayNames (BL "82" "A" _) = "82"
displayNames (BL "82" "B" _) = "82B"
displayNames (BL "ROSA" _ _) = "ROSA"
displayNames (BL "158" _ _) = "158"
displayNames _ = "??"

interestingLines :: [BusLine] -> [BusLine]
interestingLines lines = filter isInteresting lines
 where has158 = any ((==) "158" . name) lines
       isInteresting (BL n t _ ) =
           (n == "82" && t == "A") 
        || (not has158 && n == "82" && t == "B")
        || (n == "ROSA" && t == "A")
        || (n == "158" && t == "A")

-- From Gabriel Gonzalez's tweet.
leftpad :: a -> [a] -> Int -> [a]
leftpad c i n = replicate (n - length i) c ++ i

rightpad :: a -> [a] -> Int -> [a]
rightpad c i n = i ++ replicate (n - length i) c 

renderBusLine :: TimeOfDay -> BusLine -> String
renderBusLine now bl@(BL _ _ dp) =
   concatMap to5 $ (displayNames bl ++ ":"):dpts
 where to5 s = leftpad ' ' s 5
       dpts = rightpad "-" (map toDp dp) 2
       toDp t = lt - nowSecs 
        where lt = timeOfDayToTime $ read @TimeOfDay t
              nowSecs = timeOfDayToTime now
      
main :: IO ()
main = do 
    (date, time) <- getDateTime
    runInky $ do setup
                 printBusTimes ["82: - -", "ROSA: - -"]
    token <- access_token <$> getToken
    lvg <- getBusTimes löviksVägen token
    hvs <- getBusTimes hovåsNedre token
    print lvg
    print hvs
    print $ map (renderBusLine $ read @TimeOfDay time) $ interestingLines $ getLines lvg <> getLines hvs

    runInky $ do setup
                 printBusTimes ["82: - -", "ROSA: - -"]

 
