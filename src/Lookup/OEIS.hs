{-# LANGUAGE OverloadedStrings #-}

module Lookup.OEIS ( queryOEIS
                   , findSequence
                   ) where

import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Data.ByteString            (pack)
import qualified Data.ByteString.Lazy       as BS
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.List
import           Network.Wreq

queryOEIS :: String -- ^ List of integers eg. 1,2,3,4
          -> IO [C.ByteString]
-- ^ Queries OEIS to find possible functions for the sequence
queryOEIS se = do
  r <- get $ "http://oeis.org/search?fmt=text&q=" ++ se
  return $ C.lines (r ^. responseBody)

findSequence :: [Integer] -- ^ Generated list of numbers
             -> IO [String]
-- ^ Queries OEIS to find possible functions for the sequence
findSequence = queryOEIS . intercalate ",". fmap show
           >=> return . fmap C.unpack . filter ("%N" `C.isPrefixOf`)
