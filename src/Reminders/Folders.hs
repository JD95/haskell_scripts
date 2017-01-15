module Reminders.Folders ( echoLines
                         , fileReminder
                         , notifyIfFolderNotEmpty
                         ) where

import Control.Arrow
import Data.Monoid
import Control.Monad
import System.Directory
import System.Process
import qualified Data.Functor.Foldable as F
import qualified Data.Text as T

echoLines :: String -> [String] -> String
-- ^ Allows for several lines to be read out to a msg box
echoLines message = F.cata f where
  f (F.Cons l ls) = "(" <> ls <> "& echo " <> l <> ")"
  f (F.Nil) = "(echo " <> message <> ")"

fileReminder :: String -> [FilePath] -> IO ()
fileReminder _ [] = return ()
fileReminder message files = callCommand ((echoLines message files) <> "| msg * ")

notifyIfFolderNotEmpty :: String -> FilePath -> IO ()
notifyIfFolderNotEmpty message path = listDirectory path >>= fileReminder message
