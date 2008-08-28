--------------------------------------------------------------------
-- |
-- Module    : 
-- Copyright : (c) Galois, Inc. 2008
-- License   : BSD3
--
-- Maintainer: Don Stewart <dons@galois.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------


module Network.Delicious.RSS
       ( getHotlist          -- :: IO [Post]
       , getRecentBookmarks  -- :: IO [Post]
       , getTagBookmarks     -- :: Tag -> IO [Post]
       , getTagsBookmarks    -- :: [Tag] -> IO [Post]
       , getPopularBookmarks -- :: IO [Post]
       , getTagPopularBookmarks -- :: Tag -> IO [Post]
       , getSiteAlerts       -- :: IO [Post]
       , getUserBookmarks    -- :: String -> IO [Post]
       , getUserTagBookmarks -- :: String -> Tag -> IO [Post]
       , getUserTaggedBookmarks -- :: String -> [Tag] -> IO [Post]
       , getUserInfo            -- :: String -> IO [Post]
       , getUserPublicTags      -- :: String -> IO [Post]
       , getUserSubscriptions   -- :: String -> IO [Post]
       , getUserInboxBookmarks  -- :: String -> String -> IO [Post]
       , getNetworkMemberBookmarks -- :: String -> IO [Post]
       , getNetworkMemberTaggedBookmarks -- :: String -> [Tag] -> IO [Post]
       , getNetworkMembers      -- :: String -> IO [Post]
       , getNetworkFans         -- :: String -> IO [Post]
       , getURLBookmarks        -- :: URLString -> IO [Post]
{-
       , getURLSummary          -- :: URLString -> IO [Post]
-}
       ) where

import Network.Delicious.Types

import Text.Feed.Query
import Text.Feed.Types
import Text.Feed.Import

import Data.Maybe
import Data.List ( intersperse )

import Web.DAV.Client.Curl
import Web.MIME.MD5

-- ToDo:
--    * support for 'count' parameter
--    * plain/fancy

deli_base :: URLString
deli_base = "http://feeds.delicious.com/v2"

hotlist_url :: {-URL-}String
hotlist_url = deli_base ++ "/rss/"

recent_url  :: {-URL-}String
recent_url  = deli_base ++ "/rss/recent"

popular_url :: {-URL-}String
popular_url = deli_base ++ "/rss/popular"

user_url    :: {-URL-}String
user_url    = deli_base ++ "/rss/"

alert_url    :: {-URL-}String
alert_url    = deli_base ++ "/rss/alerts"

tag_url     :: {-URL-}String
tag_url     = deli_base ++ "/rss/tag/"

tags_url    :: {-URL-}String
tags_url    = deli_base ++ "/rss/tags/"

inbox_url    :: {-URL-}String
inbox_url    = deli_base ++ "/rss/inbox/"

network_url    :: {-URL-}String
network_url    = deli_base ++ "/rss/network/"

network_mem_url    :: {-URL-}String
network_mem_url    = deli_base ++ "/rss/networkmembers/"

network_fans_url    :: {-URL-}String
network_fans_url    = deli_base ++ "/rss/networkfans/"

b_url_url    :: {-URL-}String
b_url_url    = deli_base ++ "/rss/url/"

{- UNUSED:
b_urlinfo_url :: {-URL-}String
b_urlinfo_url = deli_base ++ "/rss/urlinfo/"
-}

getHotlist :: IO [Post]
getHotlist = do
  ls <- readContentsURL hotlist_url
  case parseFeedString ls of
    Nothing -> fail ("getHotlist: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getRecentBookmarks :: IO [Post]
getRecentBookmarks = do
  ls <- readContentsURL recent_url
  case parseFeedString ls of
    Nothing -> fail ("getRecentBookmarks: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getTagBookmarks :: Tag -> IO [Post]
getTagBookmarks tg = do
  ls <- readContentsURL (tag_url ++ tg)
  case parseFeedString ls of
    Nothing -> fail ("getTagBookmarks: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getTagsBookmarks :: [Tag] -> IO [Post]
getTagsBookmarks tgs = do
  ls <- readContentsURL (tag_url ++ concat (intersperse "+" tgs))
  case parseFeedString ls of
    Nothing -> fail ("getTagsBookmarks: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getPopularBookmarks :: IO [Post]
getPopularBookmarks = do
  ls <- readContentsURL popular_url
  case parseFeedString ls of
    Nothing -> fail ("getPopularBookmarks: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getTagPopularBookmarks :: Tag -> IO [Post]
getTagPopularBookmarks tg = do
  ls <- readContentsURL (popular_url ++ '/':tg)
  case parseFeedString ls of
    Nothing -> fail ("getTagPopularBookmarks: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getSiteAlerts :: IO [Post]
getSiteAlerts = do
  ls <- readContentsURL alert_url
  case parseFeedString ls of
    Nothing -> fail ("getSiteAlerts: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getUserBookmarks :: String -> IO [Post]
getUserBookmarks u = do
  ls <- readContentsURL (user_url ++ u)
  case parseFeedString ls of
    Nothing -> fail ("getUserBookmarks: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getUserTagBookmarks :: String -> Tag -> IO [Post]
getUserTagBookmarks u tg = do
  ls <- readContentsURL (user_url ++ u ++ '/':tg)
  case parseFeedString ls of
    Nothing -> fail ("getUserBookmarks: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getUserTaggedBookmarks :: String -> [Tag] -> IO [Post]
getUserTaggedBookmarks u tgs = do
  ls <- readContentsURL (user_url ++ u ++ '/':concat (intersperse "+" tgs))
  case parseFeedString ls of
    Nothing -> fail ("getUserTaggedBookmarks: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getUserInfo :: String -> IO [Post]
getUserInfo u = do
  ls <- readContentsURL (user_url ++ "userinfo/" ++ u)
  case parseFeedString ls of
    Nothing -> fail ("getUserInfo: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getUserPublicTags :: String -> IO [Post]
getUserPublicTags u = do
  ls <- readContentsURL (tags_url ++ u)
  case parseFeedString ls of
    Nothing -> fail ("getUserPublicTags: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getUserSubscriptions :: String -> IO [Post]
getUserSubscriptions u = do
  ls <- readContentsURL (user_url ++ "subscriptions/" ++ u)
  case parseFeedString ls of
    Nothing -> fail ("getUserSubscriptions: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getUserInboxBookmarks :: String -> String -> IO [Post]
getUserInboxBookmarks u key = do
  ls <- readContentsURL (inbox_url ++ u ++ "?private="++key)
  case parseFeedString ls of
    Nothing -> fail ("getUserInboxBookmarks: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getNetworkMemberBookmarks :: String -> IO [Post]
getNetworkMemberBookmarks u = do
  ls <- readContentsURL (network_url ++ u)
  case parseFeedString ls of
    Nothing -> fail ("getNetworkMemberBookmarks: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getNetworkMemberTaggedBookmarks :: String -> [Tag] -> IO [Post]
getNetworkMemberTaggedBookmarks u tgs = do
  ls <- readContentsURL (network_url ++ u ++ '/':concat (intersperse "+" tgs))
  case parseFeedString ls of
    Nothing -> fail ("getNetworkMemberTaggedBookmarks: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getNetworkMembers :: String -> IO [Post]
getNetworkMembers u = do
  ls <- readContentsURL (network_mem_url ++ u)
  case parseFeedString ls of
    Nothing -> fail ("getNetworkMembers: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getNetworkFans :: String -> IO [Post]
getNetworkFans u = do
  ls <- readContentsURL (network_fans_url ++ u)
  case parseFeedString ls of
    Nothing -> fail ("getNetworkFans: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getURLBookmarks :: URLString -> IO [Post]
getURLBookmarks url = do
  ls <- readContentsURL (b_url_url ++ md5sumStr url)
  case parseFeedString ls of
    Nothing -> fail ("getURLBookmarks: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

{- Not on offer for RSS backend:
getURLSummary :: URLString -> IO [Post]
getURLSummary url = do
  ls <- readContentsURL (b_urlinfo_url ++ md5sumStr url)
  case parseFeedString ls of
    Nothing -> fail ("getURLSummary: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))
-}

toPost :: Item -> Post
toPost i = 
 nullPost
   { postHref = fromMaybe "" (getItemLink i)
   , postDesc = fromMaybe "" (getItemTitle i)
   , postTags = getItemCategories i
   , postStamp = fromMaybe "" (getItemPublishDate i)
   }
