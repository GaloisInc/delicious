--------------------------------------------------------------------
-- |
-- Module      : Network.Delicious.User
-- Copyright   : (c) Galois, Inc. 2008
-- License     : BSD3
--
-- Maintainer  : Sigbjorn Finne <sof@galois.com>
-- Stability   : provisional
-- Portability : portable
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
import Network.Delicious.Fetch

import Text.Feed.Query
import Text.Feed.Types
import Text.Feed.Import

import Data.Maybe
import Data.List ( intercalate )

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

performCall :: String -> URLString -> DM [Post]
performCall loc u = do
  ls <- buildUrl readContentsURL u
  case parseFeedString ls of
    Nothing -> fail (loc ++ " invalid RSS feed")
    Just f  -> return (map toPost (feedItems f))

--

getHotlist :: DM [Post]
getHotlist = performCall "getHotlist" hotlist_url

getRecentBookmarks :: DM [Post]
getRecentBookmarks = performCall "getRecentBookmarks" recent_url

getTagBookmarks :: Tag -> DM [Post]
getTagBookmarks tg = performCall "getTagBookmarks" eff_url
 where
  eff_url = tag_url ++ tg

getTagsBookmarks :: [Tag] -> DM [Post]
getTagsBookmarks tgs = performCall "getTagsBookmarks" eff_url
 where
  eff_url = tag_url ++ intercalate "+" tgs

getPopularBookmarks :: DM [Post]
getPopularBookmarks = performCall "getPopularBookmarks" popular_url

getTagPopularBookmarks :: Tag -> DM [Post]
getTagPopularBookmarks tg = performCall "getTagPopularBookmarks" eff_url
 where
  eff_url = popular_url ++ '/':tg

getSiteAlerts :: DM [Post]
getSiteAlerts = performCall "getSiteAlerts" alert_url

getUserBookmarks :: String -> DM [Post]
getUserBookmarks u = performCall "getUserBookmarks" eff_url
 where
  eff_url = user_url ++ u

getUserTagBookmarks :: String -> Tag -> DM [Post]
getUserTagBookmarks u tg = performCall "getUserTagBookmarks" eff_url
 where
  eff_url = user_url ++ u ++ '/':tg

getUserTaggedBookmarks :: String -> [Tag] -> DM [Post]
getUserTaggedBookmarks u tgs = performCall "getUserTaggedBookmarks" eff_url
 where
  eff_url = user_url ++ u ++ '/':intercalate "+" tgs

getUserInfo :: String -> DM [Post]
getUserInfo u = performCall "getUserInfo" eff_url
 where
  eff_url = user_url ++ "userinfo/" ++ u

getUserPublicTags :: String -> DM [Post]
getUserPublicTags u = performCall "getUserPublicTags" eff_url
 where
  eff_url = tags_url ++ u

getUserSubscriptions :: String -> DM [Post]
getUserSubscriptions u = performCall "getUserSubscriptions" eff_url
 where
  eff_url = user_url ++ "subscriptions/" ++ u

getUserInboxBookmarks :: String -> String -> DM [Post]
getUserInboxBookmarks u key = performCall "getUserInboxBookmarks" eff_url
 where
  eff_url = inbox_url ++ u ++ "?private="++key

getNetworkMemberBookmarks :: String -> DM [Post]
getNetworkMemberBookmarks u = performCall "getNetworkMemberBookmarks" eff_url
 where
  eff_url = network_url ++ u

getNetworkMemberTaggedBookmarks :: String -> [Tag] -> DM [Post]
getNetworkMemberTaggedBookmarks u tgs = 
  performCall "getNetworkMemberTaggedBookmarks" eff_url
 where
  eff_url = network_url ++ u ++ '/':intercalate "+" tgs

getNetworkMembers :: String -> DM [Post]
getNetworkMembers u = performCall "getNetworkMembers" eff_url
 where
  eff_url = network_mem_url ++ u

getNetworkFans :: String -> DM [Post]
getNetworkFans u = performCall "getNetworkFans" eff_url
 where
  eff_url = network_fans_url ++ u

getURLBookmarks :: URLString -> DM [Post]
getURLBookmarks url = performCall "getURLBookmarks" eff_url
 where
  eff_url = b_url_url ++ hashUrl url

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
   { postHref  = fromMaybe "" (getItemLink i)
   , postDesc  = fromMaybe "" (getItemTitle i)
   , postUser  = fromMaybe "" (getItemAuthor i)
   , postTags  = getItemCategories i
   , postStamp = fromMaybe "" (getItemPublishDate i)
   }

hashUrl :: URLString -> String
hashUrl s = md5sum (pack (map (fromIntegral.fromEnum) s))

