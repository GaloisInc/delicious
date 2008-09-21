--------------------------------------------------------------------
-- |
-- Module    : Network.Delicious.User
-- Copyright : (c) Galois, Inc. 2008
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@galois.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------


module Network.Delicious.RSS
       ( getHotlist          -- :: DM [Post]
       , getRecentBookmarks  -- :: DM [Post]
       , getTagBookmarks     -- :: Tag -> DM [Post]
       , getTagsBookmarks    -- :: [Tag] -> DM [Post]
       , getPopularBookmarks -- :: DM [Post]
       , getTagPopularBookmarks -- :: Tag -> DM [Post]
       , getSiteAlerts       -- :: DM [Post]
       , getUserBookmarks    -- :: String -> DM [Post]
       , getUserTagBookmarks -- :: String -> Tag -> DM [Post]
       , getUserTaggedBookmarks -- :: String -> [Tag] -> DM [Post]
       , getUserInfo            -- :: String -> DM [Post]
       , getUserPublicTags      -- :: String -> DM [Post]
       , getUserSubscriptions   -- :: String -> DM [Post]
       , getUserInboxBookmarks  -- :: String -> String -> DM [Post]
       , getNetworkMemberBookmarks -- :: String -> DM [Post]
       , getNetworkMemberTaggedBookmarks -- :: String -> [Tag] -> DM [Post]
       , getNetworkMembers      -- :: String -> DM [Post]
       , getNetworkFans         -- :: String -> DM [Post]
       , getURLBookmarks        -- :: URLString -> DM [Post]
{-
       , getURLSummary          -- :: URLString -> DM [Post]
-}
       ) where

import Network.Delicious.Types

import Text.Feed.Query
import Text.Feed.Types
import Text.Feed.Import

import Data.Maybe
import Data.List ( intersperse )

import Network.Curl
import Data.ByteString ( pack )
import Data.Digest.OpenSSL.MD5 

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

buildUrl :: (URLString -> IO a) -> URLString -> DM a
buildUrl f u = do
  mbc <- getCount
  liftIO (f (case mbc of { Nothing -> u ; Just c ->  u++"?count="++show c}))

getHotlist :: DM [Post]
getHotlist = do
  ls <- buildUrl readContentsURL hotlist_url
  case parseFeedString ls of
    Nothing -> fail ("getHotlist: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getRecentBookmarks :: DM [Post]
getRecentBookmarks = do
  ls <- buildUrl readContentsURL recent_url
  case parseFeedString ls of
    Nothing -> fail ("getRecentBookmarks: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getTagBookmarks :: Tag -> DM [Post]
getTagBookmarks tg = do
  ls <- buildUrl readContentsURL (tag_url ++ tg)
  case parseFeedString ls of
    Nothing -> fail ("getTagBookmarks: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getTagsBookmarks :: [Tag] -> DM [Post]
getTagsBookmarks tgs = do
  ls <- buildUrl readContentsURL (tag_url ++ concat (intersperse "+" tgs))
  case parseFeedString ls of
    Nothing -> fail ("getTagsBookmarks: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getPopularBookmarks :: DM [Post]
getPopularBookmarks = do
  ls <- buildUrl readContentsURL popular_url
  case parseFeedString ls of
    Nothing -> fail ("getPopularBookmarks: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getTagPopularBookmarks :: Tag -> DM [Post]
getTagPopularBookmarks tg = do
  ls <- buildUrl readContentsURL (popular_url ++ '/':tg)
  case parseFeedString ls of
    Nothing -> fail ("getTagPopularBookmarks: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getSiteAlerts :: DM [Post]
getSiteAlerts = do
  ls <- buildUrl readContentsURL alert_url
  case parseFeedString ls of
    Nothing -> fail ("getSiteAlerts: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getUserBookmarks :: String -> DM [Post]
getUserBookmarks u = do
  ls <- buildUrl readContentsURL (user_url ++ u)
  case parseFeedString ls of
    Nothing -> fail ("getUserBookmarks: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getUserTagBookmarks :: String -> Tag -> DM [Post]
getUserTagBookmarks u tg = do
  ls <- buildUrl readContentsURL (user_url ++ u ++ '/':tg)
  case parseFeedString ls of
    Nothing -> fail ("getUserBookmarks: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getUserTaggedBookmarks :: String -> [Tag] -> DM [Post]
getUserTaggedBookmarks u tgs = do
  ls <- buildUrl readContentsURL (user_url ++ u ++ '/':concat (intersperse "+" tgs))
  case parseFeedString ls of
    Nothing -> fail ("getUserTaggedBookmarks: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getUserInfo :: String -> DM [Post]
getUserInfo u = do
  ls <- buildUrl readContentsURL (user_url ++ "userinfo/" ++ u)
  case parseFeedString ls of
    Nothing -> fail ("getUserInfo: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getUserPublicTags :: String -> DM [Post]
getUserPublicTags u = do
  ls <- buildUrl readContentsURL (tags_url ++ u)
  case parseFeedString ls of
    Nothing -> fail ("getUserPublicTags: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getUserSubscriptions :: String -> DM [Post]
getUserSubscriptions u = do
  ls <- buildUrl readContentsURL (user_url ++ "subscriptions/" ++ u)
  case parseFeedString ls of
    Nothing -> fail ("getUserSubscriptions: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getUserInboxBookmarks :: String -> String -> DM [Post]
getUserInboxBookmarks u key = do
  ls <- buildUrl readContentsURL (inbox_url ++ u ++ "?private="++key)
  case parseFeedString ls of
    Nothing -> fail ("getUserInboxBookmarks: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getNetworkMemberBookmarks :: String -> DM [Post]
getNetworkMemberBookmarks u = do
  ls <- buildUrl readContentsURL (network_url ++ u)
  case parseFeedString ls of
    Nothing -> fail ("getNetworkMemberBookmarks: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getNetworkMemberTaggedBookmarks :: String -> [Tag] -> DM [Post]
getNetworkMemberTaggedBookmarks u tgs = do
  ls <- buildUrl readContentsURL (network_url ++ u ++ '/':concat (intersperse "+" tgs))
  case parseFeedString ls of
    Nothing -> fail ("getNetworkMemberTaggedBookmarks: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getNetworkMembers :: String -> DM [Post]
getNetworkMembers u = do
  ls <- buildUrl readContentsURL (network_mem_url ++ u)
  case parseFeedString ls of
    Nothing -> fail ("getNetworkMembers: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getNetworkFans :: String -> DM [Post]
getNetworkFans u = do
  ls <- buildUrl readContentsURL (network_fans_url ++ u)
  case parseFeedString ls of
    Nothing -> fail ("getNetworkFans: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

getURLBookmarks :: URLString -> DM [Post]
getURLBookmarks url = do
  ls <- buildUrl readContentsURL (b_url_url ++ hashUrl url)
  case parseFeedString ls of
    Nothing -> fail ("getURLBookmarks: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

{- Not on offer for RSS backend:
getURLSummary :: URLString -> DM [Post]
getURLSummary url = do
  ls <- buildUrl readContentsURL (b_urlinfo_url ++ hashUrl url)
  case parseFeedString ls of
    Nothing -> fail ("getURLSummary: invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))
-}

toPost :: Item -> Post
toPost i = 
 nullPost
   { postHref = fromMaybe "" (getItemLink i)
   , postDesc = fromMaybe "" (getItemTitle i)
   , postUser = fromMaybe "" (getItemAuthor i)
   , postTags = getItemCategories i
   , postStamp = fromMaybe "" (getItemPublishDate i)
   }

readContentsURL :: URLString -> IO String
readContentsURL u = do
  let opts = [ CurlFollowLocation True
	     ]
  (_,xs) <- curlGetString u opts
  return xs

hashUrl :: URLString -> String
hashUrl s = md5sum (pack (map (fromIntegral.fromEnum) s))

