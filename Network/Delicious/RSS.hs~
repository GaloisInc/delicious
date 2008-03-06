module Delicious.RSS 
       ( getHotlist       -- :: IO [Post]
       , getPopular       -- :: IO [Post]
       , getRSSRecent     -- :: IO [Post]
       , getRSSUser       -- :: String -> IO [Post]
       , getRSSTag        -- :: String -> IO [Post]
       , getRSSUserTag    -- :: String -> String -> IO [Post]
       ) where

import Delicious.Types

import Text.Feed.Query
import Text.Feed.Types
import Text.Feed.Import

import Data.Maybe

import Web.DAV.Client.Curl

hotlist_url :: {-URL-}String
hotlist_url = "http://del.icio.us/rss/"

recent_url  :: {-URL-}String
recent_url  = "http://del.icio.us/rss/recent"

popular_url :: {-URL-}String
popular_url = "http://del.icio.us/rss/popular"

user_url    :: {-URL-}String
user_url    = "http://del.icio.us/rss/"

tag_url     :: {-URL-}String
tag_url     = "http://del.icio.us/rss/tag/"

getHotlist :: IO [Post]
getHotlist = do
  ls <- readContentsURL hotlist_url
  case parseFeedString ls of
    Nothing -> fail ("getHotlist: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getPopular :: IO [Post]
getPopular = do
  ls <- readContentsURL popular_url
  case parseFeedString ls of
    Nothing -> fail ("getPopular: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getRSSRecent :: IO [Post]
getRSSRecent = do
  ls <- readContentsURL recent_url
  case parseFeedString ls of
    Nothing -> fail ("getRSSRecent: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getRSSUser :: String -> IO [Post]
getRSSUser u = do
  ls <- readContentsURL (user_url ++ u)
  case parseFeedString ls of
    Nothing -> fail ("getRSSUser: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getRSSTag :: String -> IO [Post]
getRSSTag tg = do
  ls <- readContentsURL (tag_url ++ tg)
  case parseFeedString ls of
    Nothing -> fail ("getRSSTag: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getRSSUserTag :: String -> String -> IO [Post]
getRSSUserTag u tg = do
  ls <- readContentsURL (tag_url ++ u ++ '/':tg)
  case parseFeedString ls of
    Nothing -> fail ("getRSSUserTag: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

toPost :: Item -> Post
toPost i = 
 nullPost
   { postHref = fromMaybe "" (getItemLink i)
   , postDesc = fromMaybe "" (getItemTitle i)
   , postTags = getItemCategories i
   , postStamp = fromMaybe "" (getItemPublishDate i)
   }


