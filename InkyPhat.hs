{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module InkyPhat ( runInky, runInkyLoop
                  -- commands
                , text, paste, display, setRotation, clear

                -- Data
                , image, size, dimensions, readString
                -- Types
                , Color (..) , Font (..), Image
                , InkyException (..), InkyIO
    ) where 


import System.Process
import Control.Concurrent
import GHC.IO.Handle (Handle, hClose, hFlush, hFlushAll, hWaitForInput)
import Data.Maybe (fromMaybe)
import Control.Monad (join)

import Control.Monad.Reader
import Control.Monad.Except
import Data.Text
import qualified Data.Text as T

import Prelude hiding ( hPutStrLn, hGetLine )
import Data.Text.IO

import System.IO.Error

import Text.Read (readEither)

data InkyException = ValueParseError Text
                   | PythonError Text
                   | NoDataError deriving (Show, Eq)

type InkyIO = ExceptT InkyException (ReaderT (Handle, Handle, Handle) IO)

-- To avoid issues with FFI, we'll just go the easy
-- interpeter way.
sendCommand :: Text -> InkyIO ()
sendCommand cmd =
    do (stdin, _, _) <- ask
       dlog cmd
       liftIO $ do hPutStrLn stdin cmd
                   hFlushAll stdin

dlog :: Text -> InkyIO ()
# if debug
dlog = putStrLn
# else
dlog _ = return ()
#endif

readValue :: Read a => Text -> InkyIO a
readValue cmd  = do val <- readEither . unpack <$> readString cmd
                    case val of
                      Left err -> throwError (ValueParseError $ pack err)
                      Right v -> return v


readString :: Text -> InkyIO Text
readString cmd =
  do (stdin, stdout, stderr) <- ask
     liftIO $ hFlushAll stdout
     sendCommand $ "print(" <> cmd <> ")"
     --- We wait at max 30 seconds for output
     val <- liftIO $ readWithTimeout stdout (30*1000)
     case val of
        Just v -> do dlog v
                     return v
        _ -> do err <- liftIO $ readWithTimeout stderr (5 * 1000)
                case err of
                  Just e -> throwError (PythonError e)
                  _ -> throwError NoDataError
 where
    readWithTimeout :: Handle -> Int -> IO (Maybe Text)
    readWithTimeout handle timeout
     = do hasOutput <- flip catchIOError (\_->return False)
                        $ hWaitForInput handle timeout
          if hasOutput then Just <$> hGetLine handle
                       else return Nothing
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

runInkyLoop :: InkyIO a -> IO ()
runInkyLoop loop
 = do r <- runInky loop
      case r of
        Left err -> print err
        _ -> return ()
      runInkyLoop loop



runInky :: InkyIO a -> IO (Either InkyException a)
runInky action =
  do (Just stdin, Just stdout , Just stderr, proc)
        <- createProcess $ cp { std_in = CreatePipe, std_out = CreatePipe
                              , std_err = CreatePipe }
     r <- flip runReaderT (stdin, stdout, stderr) $
            runExceptT $
              do mapM_ sendCommand initialCommands
                 action
     terminateProcess proc
     mapM_ hClose [stdin, stdout, stderr]
     return r
  where cp = shell "python3 -i"
