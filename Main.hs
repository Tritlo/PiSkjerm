{-# LANGUAGE TypeApplications #-}
module Main where

import BusTimes
import InkyPhat

import Data.Maybe
import Data.Time
import Data.List (intercalate)
import Control.Monad.Reader (lift)

löviksVägen :: BusStop
löviksVägen = 9021014004663000
hovåsNedre :: BusStop
hovåsNedre = 9021014003235000

setup :: InkyIO Image
setup = setRotation 180 >> image "vt" "vt.png"

fontheight :: Int
fontheight = 12
fontwidth = fontheight
linespace :: Int
linespace = 2


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
   unwords $ map to5 $ (displayNames bl ++ ":"):dpts
 where to5 s = leftpad ' ' s 5
       dpts = rightpad "-" (map toDp dp) 2
       toDp t = msg 
         where mins = floor (lt - nowSecs) `div` 60
               msg = if mins > 0 then (show mins) ++ "min" else "Núna!"
               lt = timeOfDayToTime $ read @TimeOfDay (t ++ ":00")
               nowSecs = timeOfDayToTime now

printBusTimes :: [String] -> InkyIO ()
printBusTimes msgs =
  do (inkyW, inkyH) <- dimensions
     let (x,y) = (2, (inkyH `div` 2 - fontheight*(length msgs) `div` 2))
         pr (m,i) = text pos m Nothing Nothing  
          where pos = (x, y + i*(fontheight + linespace))
     mapM_ pr $ zip msgs [0.. ]
  where pos = fromMaybe

printTime :: InkyIO ()
printTime = do (date, time) <- lift getDateTime
               (inkyW, _) <- dimensions
               let pos = ((inkyW - 16*fontwidth) `div` 2,18)
               text pos (unwords [date, time]) (Just Red) Nothing

printName :: InkyIO ()
printName = do (inkyW, _) <- dimensions
               let pos = ((inkyW - 14*fontwidth) `div` 2,2)
               text pos "Strætóferðir" (Just Red) (Just (Font 14))

printLogo :: Image -> InkyIO ()
printLogo img
 = do (inkyW, inkyH) <- dimensions
      (logoW, logoH) <- size img
      let (vx,vy) =  ((inkyW - 10*14 - logoW -10) `div` 2, inkyH -2-14)
          posText = (vx + logoW+2, vy -4)
          posLogo = (vx, vy-logoH+14-2)
      text posText "Västtrafik" (Just Red) (Just (Font 14))
      paste img posLogo
  

updateDisplay :: Image -> [String] -> InkyIO ()
updateDisplay img times
 = do { printName
      ; printTime
      ; printBusTimes times
      ; printLogo img
      ; display }


main :: IO ()
main = do 
    (date, time) <- getDateTime
    token <- access_token <$> getToken
    lvg <- getBusTimes löviksVägen token
    hvs <- getBusTimes hovåsNedre token
    print lvg
    print hvs
    let busTimes = map (renderBusLine $ read @TimeOfDay (time ++ ":00"))
                     $ interestingLines $ getLines lvg <> getLines hvs
    runInky $ do img <- setup
                 updateDisplay img busTimes 

 
