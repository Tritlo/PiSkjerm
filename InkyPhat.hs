module InkyPhat (
    runInky, InkyIO,
    -- commands
    text, paste, display, setRotation,

    -- Data
    image, size, dimensions
    -- Types
    , Color (..) , Font (..), Image
    ) where 


import System.Process
import Control.Concurrent
import GHC.IO.Handle
import Data.Maybe (fromMaybe)
import Control.Monad (join)
import Data.List (intercalate)

import Control.Monad.Reader

type InkyIO = ReaderT (Handle, Handle) IO



-- To avoid issues with FFI, we'll just go the easy
-- interpeter way.
sendCommand :: String -> InkyIO ()
sendCommand cmd =
    do (stdin, _) <- ask
       lift $ do hPutStr stdin (cmd ++ "\n")
                 hFlushAll stdin

readValue :: Read a => String -> InkyIO a
readValue cmd =
  do (stdin, stdout) <- ask
     lift $ hFlushAll stdout
     sendCommand cmd 
     lift $ read <$> hGetLine stdout

initialCommands :: [String]
initialCommands
    = [ "import inkyphat" ]

data Color = Black | White | Red deriving (Show)

class InkyPhatVal a where
 toIPVal :: a -> String

instance InkyPhatVal Color where
  toIPVal Black = "inkyphat.BLACK"
  toIPVal White = "inkyphat.WHITE"
  toIPVal Red   = "inkyphat.RED"

-- We only have the one font for now
data Font = Font Int 

instance InkyPhatVal Font where
  toIPVal (Font a) = "inkyphat.ImageFont.truetype(inkyphat.fonts.PressStart2P," ++ show a ++")"

display :: InkyIO ()
display = sendCommand "inkyphat.show()"

clear :: InkyIO ()
clear = sendCommand  "inkyphat.clear()"

type Image = String

setRotation :: Int -> InkyIO ()
setRotation rot = sendCommand $ "inkyphat.set_rotation(" ++ show rot ++ ")"

image :: String -> String -> InkyIO Image
image name loc = do sendCommand $ join [ name
                                       , " = "
                                       , "inkyphat.Image.open('"
                                       , loc
                                       , "')"]
                    return name


paste :: Image -> (Int, Int) -> InkyIO ()
paste img loc = 
  sendCommand $ intercalate "," $ ["inkyphat.paste(" ++ img
                                  , show loc ++ ")"]

text :: (Int, Int) -> String -> Maybe Color -> Maybe Font -> InkyIO ()
text xy text color font = 
    sendCommand $ intercalate "," ["inkyphat.text(" ++ show xy
                                  , show text
                                  , toIPVal col
                                  , toIPVal fon ++ ")" ]
  where fon = fromMaybe (Font 12) font
        col =  fromMaybe Black color

size :: Image -> InkyIO (Int, Int)
size img = readValue $ "(" ++ img ++ ".size[0], " ++ img ++ ".size[1])"

dimensions :: InkyIO (Int, Int)
dimensions = readValue "(inkyphat.WIDTH, inkyphat.HEIGHT)"

runInky :: InkyIO a -> IO a 
runInky action =
  do (Just stdin, Just stdout , _, proc) <- createProcess $ cp { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
     r <- flip runReaderT (stdin, stdout) $
            do mapM_ sendCommand initialCommands
               action
     mapM_ hClose [stdin, stdout]
     return r
  where cp = shell "python3 -i"
