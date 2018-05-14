{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module InkyPhat ( runInky, InkyIO
                  -- commands
                , text, paste, display, setRotation, clear

                -- Data
                , image, size, dimensions, readString
                -- Types
                , Color (..) , Font (..), Image
    ) where 


import System.Process
import Control.Concurrent
import GHC.IO.Handle (Handle, hClose, hFlush, hFlushAll, hWaitForInput)
import Data.Maybe (fromMaybe)
import Control.Monad (join)

import Control.Monad.Reader
import Data.Text
import qualified Data.Text as T

import Prelude hiding ( hPutStrLn, hGetLine )
import Data.Text.IO

type InkyIO = ReaderT (Handle, Handle) IO

-- To avoid issues with FFI, we'll just go the easy
-- interpeter way.
sendCommand :: Text -> InkyIO ()
sendCommand cmd =
    do (stdin, _) <- ask
       dlog cmd
       lift $ do hPutStrLn stdin cmd
                 hFlushAll stdin

dlog :: Text -> InkyIO ()
# if debug
dlog = putStrLn
# else
dlog _ = return ()
#endif

readValue :: Read a => Text -> InkyIO a
readValue = fmap (read . unpack) . readString

readString :: Text -> InkyIO Text
readString cmd =
  do (stdin, stdout) <- ask
     lift $ hFlushAll stdout
     sendCommand $ "print(" <> cmd <> ")"
     --- We wait at max 30 seconds for output
     val <- lift $ do  hasOutput <- hWaitForInput stdout (30*1000)
                       if hasOutput then hGetLine stdout else return ""
     dlog val
     return val

initialCommands :: [Text]
initialCommands
    = [ "import inkyphat"
      , "from urllib import request"
      , "from datetime import datetime"]

data Color = Black | White | Red deriving (Show)

class InkyPhatVal a where
 toIPVal :: a -> Text

instance InkyPhatVal Color where
  toIPVal Black = "inkyphat.BLACK"
  toIPVal White = "inkyphat.WHITE"
  toIPVal Red   = "inkyphat.RED"

-- We only have the one font for now
newtype Font = Font Int

instance InkyPhatVal Font where
  toIPVal (Font a) =
    "inkyphat.ImageFont.truetype(inkyphat.fonts.PressStart2P,"
      <> pack (show a) <>")"

display :: InkyIO ()
display = sendCommand "inkyphat.show()"

clear :: InkyIO ()
clear = sendCommand "inkyphat.clear()"

type Image = Text

setRotation :: Int -> InkyIO ()
setRotation rot =
  sendCommand $ "inkyphat.set_rotation(" <> pack (show rot) <> ")"

image :: Text -> Text -> InkyIO Image
image name loc =
  do sendCommand $ name <> " = inkyphat.Image.open('" <> loc <> "')"
     return name


paste :: Image -> (Int, Int) -> InkyIO ()
paste img loc =
  sendCommand $ intercalate ","  ["inkyphat.paste(" <> img
                                 , pack (show loc) <> ")"]

text :: (Int, Int) -> Text -> Maybe Color -> Maybe Font -> InkyIO ()
text xy text color font =
    sendCommand $ intercalate "," ["inkyphat.text(" <> pack (show xy)
                                  , "'" <> text <> "'"
                                  , toIPVal col
                                  , toIPVal fon <> ")" ]
  where fon = fromMaybe (Font 12) font
        col =  fromMaybe Black color

size :: Image -> InkyIO (Int, Int)
size img = readValue $ "(" <> img <> ".size[0], " <> img <> ".size[1])"

dimensions :: InkyIO (Int, Int)
dimensions = readValue "(inkyphat.WIDTH, inkyphat.HEIGHT)"

runInky :: InkyIO a -> IO a
runInky action =
  do (Just stdin, Just stdout , Just stderr, proc)
        <- createProcess $ cp { std_in = CreatePipe, std_out = CreatePipe
                              , std_err = CreatePipe }
     r <- flip runReaderT (stdin, stdout) $
            do mapM_ sendCommand initialCommands
               action
     mapM_ hClose [stdin, stdout, stderr]
     return r
  where cp = shell "python3 -i"
