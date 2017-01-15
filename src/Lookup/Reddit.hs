{-# LANGUAGE OverloadedStrings #-}

module Lookup.Reddit () where

import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import Data.Default.Class
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock
import Data.Time.Calendar

import Reddit
import Reddit.Types.Post
import Reddit.Types.Listing

tshow :: Show a => a -> T.Text
tshow = T.pack . show

type RedditResult m a = m (Either (APIError RedditError) a)

fmap' f = fmap (fmap f)
fmap'' f = fmap' (fmap f)

pullPostsFrom :: MonadIO m => T.Text -> RedditResult m [Post]
pullPostsFrom = runRedditAnon . fmap contents . getPosts' def Top . Just . R

nPostsFrom :: MonadIO m => Int -> T.Text -> RedditResult m [Post]
nPostsFrom n subName = fmap (take n <$>) (pullPostsFrom subName)

postsSince :: UTCTime -> [Post] -> [Post]
postsSince date = filter ((<) date . created)

postsForThisWeek :: MonadIO m => T.Text -> RedditResult m [Post]
postsForThisWeek subName = do
    UTCTime d t <- liftIO getCurrentTime
    posts <- pullPostsFrom subName
    return $ postsSince (UTCTime (addDays (-7) d) t) <$> posts
    
titlesFromThisWeek :: MonadIO m => T.Text -> RedditResult m [T.Text]
titlesFromThisWeek = fmap' (fmap title) . postsForThisWeek
