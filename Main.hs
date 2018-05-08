{-# LANGUAGE TypeApplications, OverloadedStrings, CPP #-}
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
                        , filter, null, take)
import qualified Data.Text as T
import Control.Concurrent
import Data.Text.Encoding as E
import System.Environment

import Data.List (sort)


-- We have a flag in the cabal file that allows us to chose between using the
-- python interpreter for time and network queries or using a native haskell
-- implementation. However, the native implementation segfaults when run on
-- the device, due to a bug in the cross-compilation toolchain, hence the
-- possiblity to choose.
# if PYTHON_HACKS
import PythonHacks
# else
import ReqNetworking
# endif

getToken :: InkyIO (Maybe TokenResponse)
getDateTime :: InkyIO (Text, Text)
getBusTimes :: Token -> BusStop -> InkyIO (Maybe BusResponse)
# if PYTHON_HACKS
getToken = pyGetToken
getDateTime = pyGetDateTime
getBusTimes = pyGetBusTimes
#else
getToken = lift hsGetToken
getDateTime = lift hsGetDateTime
getBusTimes t s = lift (hsGetBusTimes t s)
#endif


löviksVägen :: BusStop
löviksVägen = 9021014004663000
hovåsNedre :: BusStop
hovåsNedre = 9021014003235000

nearby :: [BusStop]
nearby = [löviksVägen, hovåsNedre]

setup :: InkyIO Image
setup = setRotation 180 >> image "vt" "vt.png"

fontheight :: Int
fontheight = 12
fontwidth = fontheight
linespace :: Int
linespace = 2


-- From Gabriel Gonzalez's tweet.
leftpad :: Char -> Text -> Int -> Text
leftpad c i n = T.replicate (n - T.length i) (singleton c) <> i

rightpad :: a -> [a] -> Int -> [a]
rightpad c i n = i ++ replicate (n - length i) c 

printBusTimes :: [Text] -> InkyIO ()
printBusTimes msgs =
  do (inkyW, inkyH) <- dimensions
     let (x,y) = (2, inkyH `div` 2 - fontheight * length msgs `div` 2)
         pr (m,i) = text pos m Nothing Nothing  
          where pos = (x, y + i*(fontheight + linespace))
     mapM_ pr $ zip msgs [0.. ]
  where pos = fromMaybe

printTime :: InkyIO ()
printTime = do (date, time) <- getDateTime
               (inkyW, _) <- dimensions
               let pos = ((inkyW - 16*fontwidth) `div` 2,18)
               text pos (T.unwords [date, time]) (Just Red) Nothing

printName :: InkyIO ()
printName = do (inkyW, _) <- dimensions
               let pos = ((inkyW - 14*fontwidth) `div` 2,2)
               -- Strætóferðir means "Bus Trips"
               text pos "Strætóferðir" (Just Red) (Just (Font 14))

printLogo :: Image -> InkyIO ()
printLogo img
 = do (inkyW, inkyH) <- dimensions
      (logoW, logoH) <- size img
      let (vx,vy) =  ((inkyW - 10*14 - logoW -10) `div` 2, inkyH -2-14)
          posText = (vx + logoW+2, vy -4)
          posLogo = (vx, vy-logoH+14-2)
      -- Västtrafik is the name of the Gothenburg transport authority.
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

-- Note [Known Lines]
-- We only want to show known lines, i.e. those that we take from our house,
-- and we want to display 82 first, then Rosa, and then the others.
-- Additionally, we want to show the 158 when it is running (in the morning),
-- but otherwise show the 82 going to Brottkärr, since we can only fit 3
-- lines comfortably on the display at once. This is why we do `take 3`
-- when choosing what to output, which selects the 158 if availble, but
-- the 82B otherwise.
data KnownLine = B82 [String]
               | BRosa [String]
               | B158 [String]
               | B82B [String] deriving (Eq, Ord)

-- These are the lines going in the right direction that we are interested in.
toKnownLine :: BusLine -> Maybe KnownLine
toKnownLine (BL "82" "A" d) = Just $ B82 d
toKnownLine (BL "82" "B" d) = Just $ B82B d
toKnownLine (BL "ROSA" "A" d) = Just $ BRosa d
toKnownLine (BL "158" "A" d) = Just $ B158 d
toKnownLine (BL n t d) = Nothing

knownDepartures :: KnownLine -> [String]
knownDepartures (B82 d) = d
knownDepartures (B82B d) = d
knownDepartures (BRosa d) = d
knownDepartures (B158 d) = d

instance Show KnownLine where
  show (B82 _) = "82"
  show (B82B _) = "82B"
  show (BRosa _) = "ROSA"
  show (B158 _) = "158"

renderLine :: TimeOfDay -> KnownLine -> Text
renderLine now kl =
   T.unwords $ map to5 $ [pack (show kl) <> ":"] <> dpts
 where to5 s = leftpad ' ' s 5
       dpts = rightpad "-" (map toDp $ knownDepartures kl) 2
       -- We want to show how many minutes there are until the bus departs.
       -- Note that we check for > 0, since the date might be in the past
       -- if the bus is waiting at the stop or too much time has passed
       -- since the request was made.
       toDp t = if mins > 0 then pack (show mins) <> "min"
                else "Núna!" -- "Núna" means "Right now".
         where mins = floor (lt - nowSecs) `div` 60
               lt = timeOfDayToTime $ read @TimeOfDay (t <> ":00")
               nowSecs = timeOfDayToTime now

loop :: Image -> InkyIO ()
loop img =
  do (date, time) <- getDateTime
     -- We could save the token and re-use it if it hasn't expired, but since
     -- the performance is limited by the screen refresh time anyway, we don't
     -- bother.
     mb_token <- fmap access_token <$> getToken
     case mb_token of
       Just token ->
         do stops <- catMaybes <$> mapM (getBusTimes token) nearby
            let now = read @TimeOfDay ( unpack time ++ ":00")
                -- See Note [Known Lines]
                toDisplay = take 3 $ sort $ mapMaybe toKnownLine
                              $ concatMap getLines stops
                busTimes = map (renderLine now) toDisplay
                -- "Engar ferðir núna" means "No trips right now", which we
                -- display when the buses have stopped running (usually quite
                -- late at night).
                msg = if null busTimes then ["Engar ferðir núna!"] else busTimes
            updateDisplay img msg
       _ -> return ()
     lift $ threadDelay $ 60 * 1000 * 1000
     loop img

main :: IO ()
main = runInky $ setup >>= loop
