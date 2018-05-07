{-# LANGUAGE TypeApplications, OverloadedStrings #-}
module Main where

import BusTimes
import InkyPhat

import Data.Maybe
import Control.Monad.Reader (lift)
import Data.Aeson
import Data.Time
import qualified Data.ByteString.Lazy.Char8 as B (pack)
import Data.Text hiding ( map, concatMap, zip
                        , length, replicate, any
                        , filter, null)
import qualified Data.Text as T
import Control.Concurrent
import Data.Text.Encoding as E


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


displayNames :: BusLine -> Text
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
leftpad :: Char -> Text -> Int -> Text
leftpad c i n = T.replicate (n - T.length i) (singleton c) <> i

rightpad :: a -> [a] -> Int -> [a]
rightpad c i n = i ++ replicate (n - length i) c 

renderBusLine :: TimeOfDay -> BusLine -> Text
renderBusLine now bl@(BL _ _ dp) =
   T.unwords $ map to5 $ [displayNames bl <> ":"] <> dpts
 where to5 s = leftpad ' ' s 5
       dpts = rightpad "-" (map toDp dp) 2
       toDp t = msg 
         where mins = floor (lt - nowSecs) `div` 60
               msg = if mins > 0 then pack (show mins) <> "min" else "Núna!"
               lt = timeOfDayToTime $ read @TimeOfDay (t <> ":00")
               nowSecs = timeOfDayToTime now

printBusTimes :: [Text] -> InkyIO ()
printBusTimes msgs =
  do (inkyW, inkyH) <- dimensions
     let (x,y) = (2, inkyH `div` 2 - fontheight * length msgs `div` 2)
         pr (m,i) = text pos m Nothing Nothing  
          where pos = (x, y + i*(fontheight + linespace))
     mapM_ pr $ zip msgs [0.. ]
  where pos = fromMaybe

printTime :: InkyIO ()
printTime = do (date, time) <- pyGetTime
               (inkyW, _) <- dimensions
               let pos = ((inkyW - 16*fontwidth) `div` 2,18)
               text pos (T.unwords [date, time]) (Just Red) Nothing

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
  

updateDisplay :: Image -> [Text] -> InkyIO ()
updateDisplay img times
 = do { clear
      ; printName
      ; printTime
      ; printBusTimes times
      ; printLogo img
      ; display }

-- | We use these to get the information via Python,
-- since the network libraries segfault on the pi.
pyGetToken :: InkyIO (Maybe TokenResponse)
pyGetToken = do (key, secret) <- lift getAuthCredentials 
                let auth = authToken key secret
                    url = "https://api.vasttrafik.se/token"
                    body = "grant_type=client_credentials"
                decodeStrict . E.encodeUtf8 <$> urlRequest url [] (Basic auth) body

pyGetBusTimes :: (Text, Text) -> BusStop -> Token -> InkyIO (Maybe BusResponse)
pyGetBusTimes (date, time) stop token =
   do res <- E.encodeUtf8 <$> urlRequest url params (Bearer token)  ""
      case eitherDecodeStrict res of
        Left err -> do lift $ do putStrLn (err ++ " when decoding:")
                                 print res
                       return Nothing
        Right v -> return v
  where url = "https://api.vasttrafik.se/bin/rest.exe/v2/departureBoard"
        auth = Bearer token
        params = [("id", pack $ show stop)
                 , ("maxDeparturesPerLine", pack $ show 2)
                 , ("format","json")
                 , ("timeSpan", pack $ show 59)
                 , ("date", date)
                 , ("time", time)]


loop :: Image -> InkyIO ()
loop img =
  do (date, time) <- pyGetTime
     token <- fmap access_token <$> pyGetToken
     case token of
       Just token ->
         do stops <- catMaybes <$>
                       mapM (flip (pyGetBusTimes (date,time)) token)
                         [löviksVägen, hovåsNedre]
            let now = read @TimeOfDay ( unpack time ++ ":00")
                interesting = interestingLines $ concatMap getLines stops
                busTimes = map (renderBusLine now) interesting
                msg = if null busTimes
                      then ["Engar ferðir núna!"]
                      else busTimes
            updateDisplay img msg
       Nothing -> return ()
     lift $ threadDelay $ 60 * 1000000 
     loop img

main :: IO ()
main = runInky $ setup >>= loop

 
