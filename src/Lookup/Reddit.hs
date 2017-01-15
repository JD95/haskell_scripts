{-# LANGUAGE OverloadedStrings #-}

module Lookup.Reddit () where

import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import Data.Default.Class
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Reddit
import Reddit.Types.Post
import Reddit.Types.Listing

tshow :: Show a => a -> Text.Text
tshow = Text.pack . show

test :: MonadIO m => RedditT m ()
test = do
  Listing _ _ posts <- getPosts' def Top (Just . R $ "technology")
  forM_ (take 10 $ posts) $ \post -> do
    liftIO $ Text.putStrLn (title post)

testr :: IO ()
testr = do
    r <- runRedditAnon test
    print r
