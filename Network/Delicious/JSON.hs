--------------------------------------------------------------------
-- |
-- Module    : Network.Delicious.JSON
-- Copyright : (c) Galois, Inc. 2008
-- License   : BSD3
--
-- Maintainer: Don Stewart <dons@galois.com>
-- Stability : provisional
-- Portability:
--
-- Access del.icio.us JSON services.
--
-- See <http://del.icio.us/help/json/> for more details on the API.
--
-- "You can use JSON feeds at del.icio.us to fetch, remix, and mashup a
-- variety of data for use in your own custom applications and
-- browser-based presentation styles."
--

module Network.Delicious.JSON 
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
       , getURLSummary          -- :: URLString -> IO URLDetails
       , getURLDetails          -- :: URLString -> IO URLDetails

       , URLDetails(..)
       ) where

import Text.JSON
import Text.JSON.Types

import Network.Delicious.Types

import Control.Monad
import Data.List
import Data.Ord
import Data.Char
import Data.Maybe
 
import Web.MIME.MD5 (md5sumStr)
import Web.DAV.Client.Curl

------------------------------------------------------------------------

-- | Retrieve tags associated with a url from delicious. 
-- An example, extract the tags associated with 'xmonad':
--
-- > > getURLSummary "http://xmonad.org/"
-- >
-- >       (URLDetails {total = 283
-- >                   ,tags = [("haskell",176)
-- >                           ,("windowmanager",133)
-- >                           ,("x11",126)
-- >                           ,("linux",116)
-- >                           ,("wm",74)
-- >                           ,("software",55)
-- >                           ,("gui",39)
-- >                           ,("desktop",26)
-- >                           ,("programming",25)
-- >                           ,("opensource",23)
-- >                           ,("xmonad",20)]
-- >                   }
--
getURLDetails :: String -> IO URLDetails
getURLDetails uarl = getURLSummary uarl

baseUrl :: String
baseUrl = "http://feeds.delicious.com/v2/json"

------------------------------------------------------------------------

getHotlist :: IO [Post]
getHotlist = do
    s <- readContentsURL hot_url
    case decodeStrict s of
      Ok e    -> return e
      Error e -> ioError $ userError ("getHotlist: " ++ e)

  where hot_url = baseUrl

getRecentBookmarks :: IO [Post]
getRecentBookmarks = do
    s <- readContentsURL rec_url
    case decodeStrict s of
      Ok e    -> return e
      Error e -> ioError $ userError ("getRecent: " ++ e)

  where rec_url = baseUrl ++ "/recent"

getTagBookmarks :: Tag -> IO [Post]
getTagBookmarks tg = do
    ls <- readContentsURL eff_url
    case decodeStrict ls of
      Ok s    -> return s
      Error e -> ioError $ userError ("getTagBookmarks: " ++ e)

  where eff_url = baseUrl ++ "/tag/" ++ tg

getTagsBookmarks    :: [Tag] -> IO [Post]
getTagsBookmarks tgs = do
    ls <- readContentsURL eff_url
    case decodeStrict ls of
      Ok s    -> return s
      Error s -> ioError $ userError ("getTagsBookmarks: " ++ s)

  where eff_url = baseUrl ++ "/tag/" ++ concat (intersperse "+" tgs)

getPopularBookmarks :: IO [Post]
getPopularBookmarks = do
    ls <- readContentsURL eff_url
    case decodeStrict ls of
      Ok s    -> return s
      Error s -> ioError $ userError ("getPopularBookmarks: " ++ s)

  where eff_url = baseUrl ++ "/popular"

getTagPopularBookmarks :: Tag -> IO [Post]
getTagPopularBookmarks tg = do
    ls <- readContentsURL eff_url
    case decodeStrict ls of
      Ok s    -> return s
      Error s -> ioError $ userError ("getTagPopularBookmarks: " ++ s)

  where eff_url = baseUrl ++ "/popular/" ++ tg

getSiteAlerts       :: IO [Post]
getSiteAlerts = do
    ls <- readContentsURL eff_url
    case decodeStrict ls of
      Ok s    -> return s
      Error s -> ioError $ userError ("getTagPopularBookmarks: " ++ s)

  where eff_url = baseUrl ++ "/alerts"

getUserBookmarks    :: String -> IO [Post]
getUserBookmarks usr = do
    ls <- readContentsURL eff_url
    case decodeStrict ls of
      Ok s    -> return s
      Error s -> ioError $ userError ("getUserBookmarks: " ++ s)

  where eff_url = baseUrl ++ '/':usr

getUserTagBookmarks :: String -> Tag -> IO [Post]
getUserTagBookmarks usr tg = do
    ls <- readContentsURL eff_url
    case decodeStrict ls of
      Ok s    -> return s
      Error s -> ioError $ userError ("getUserTagBookmarks: " ++ s)

  where eff_url = baseUrl ++ '/':usr++'/':tg

getUserTaggedBookmarks :: String -> [Tag] -> IO [Post]
getUserTaggedBookmarks usr tgs = do
    ls <- readContentsURL eff_url
    case decodeStrict ls of
      Ok s    -> return s
      Error s -> ioError $ userError ("getUserTaggedBookmarks: " ++ s)

  where eff_url = baseUrl ++ '/':usr++'/':concat (intersperse "+" tgs)

getUserInfo :: String -> IO [Post]
getUserInfo usr = do
    ls <- readContentsURL eff_url
    case decodeStrict ls of
      Ok s    -> return s
      Error s -> ioError $ userError ("getUserInfo: " ++ s)

  where eff_url = baseUrl ++ "/userinfo/" ++ usr

getUserPublicTags      :: String -> IO [Post]
getUserPublicTags usr = do
    ls <- readContentsURL eff_url
    case decodeStrict ls of
      Ok s    -> return s
      Error s -> ioError $ userError ("getUserPublicTags: " ++ s)

  where eff_url = baseUrl ++ "/tags/" ++ usr


getUserSubscriptions   :: String -> IO [Post]
getUserSubscriptions usr = do
    ls <- readContentsURL eff_url
    case decodeStrict ls of
      Ok s    -> return s
      Error s -> ioError $ userError ("getUserSubscriptions: " ++ s)

  where eff_url = baseUrl ++ "/subscriptions/" ++ usr

getUserInboxBookmarks  :: String -> String -> IO [Post]
getUserInboxBookmarks usr k = do
    ls <- readContentsURL eff_url
    case decodeStrict ls of
      Ok s    -> return s
      Error s -> ioError $ userError ("getUserInboxBookmarks: " ++ s)

  where eff_url = baseUrl ++ "/inbox/" ++ usr ++ "?private="++k

getNetworkMemberBookmarks :: String -> IO [Post]
getNetworkMemberBookmarks usr = do
    ls <- readContentsURL eff_url
    case decodeStrict ls of
      Ok s    -> return s
      Error s -> ioError $ userError ("getNetworkMemberBookmarks: " ++ s)

  where eff_url = baseUrl ++ "/network/" ++ usr

getNetworkMemberTaggedBookmarks :: String -> [Tag] -> IO [Post]
getNetworkMemberTaggedBookmarks usr tgs = do
    ls <- readContentsURL eff_url
    case decodeStrict ls of
      Ok s    -> return s
      Error s -> ioError $ userError ("getNetworkMemberTaggedBookmarks: " ++ s)

  where eff_url = baseUrl ++ "/network/" ++ usr ++ '/':concat (intersperse "+" tgs)


getNetworkMembers :: String -> IO [Post]
getNetworkMembers usr = do
    ls <- readContentsURL eff_url
    case decodeStrict ls of
      Ok s    -> return s
      Error s -> ioError $ userError ("getNetworkMembers: " ++ s)

  where eff_url = baseUrl ++ "/networkmembers/" ++ usr

getNetworkFans         :: String -> IO [Post]
getNetworkFans usr = do
    ls <- readContentsURL eff_url
    case decodeStrict ls of
      Ok s    -> return s
      Error s -> ioError $ userError ("getNetworkFans: " ++ s)

  where eff_url = baseUrl ++ "/networkfans/" ++ usr

getURLBookmarks  :: URLString -> IO [Post]
getURLBookmarks turl = do
    ls <- readContentsURL eff_url
    case decodeStrict ls of
      Ok s    -> return s
      Error s -> ioError $ userError ("getURLBookmarks: " ++ s)

  where eff_url = baseUrl ++ "/url/" ++ md5sumStr turl

getURLSummary :: URLString -> IO URLDetails
getURLSummary turl = do
    ls <- readContentsURL eff_url
    case decodeStrict ls of
      Ok s    -> return s
      Error s -> ioError $ userError ("getURLSummary: " ++ s)

  where eff_url = baseUrl ++ "/urlinfo/" ++ md5sumStr turl

------------------------------------------------------------------------

-- | A structure represening the the delicious tags associated with a url.
data URLDetails =
        URLDetails { total :: !Integer
                   , tags  :: [(String,Integer)]
		   , hash  :: String {-MD5-}
		   , url   :: String {-URL-}
		   }
        deriving (Eq,Show,Read)

nullURLDetails :: URLDetails
nullURLDetails = 
  URLDetails { total = 0
             , tags  = []
	     , hash  = ""
	     , url   = ""
	     }

-- | Compose and decompose URLDetails as JSON in the form delicious uses.
instance JSON URLDetails where
    showJSON ud = JSObject $ toJSObject
        [ ("hash",        showJSON (JSONString (hash ud)))
        , ("total_posts", showJSON (total ud))
        , ("top_tags",    JSObject $ toJSObject
                            [(x,showJSON y) | (x,y) <- tags ud ])
        , ("url",         showJSON (JSONString (url ud)))
        ]

    readJSON (JSArray []) = return nullURLDetails
    readJSON (JSArray [x]) = readJSON x
    readJSON (JSObject (JSONObject pairs))
        = do the_tags <- case lookup "top_tags" pairs of
                        Nothing -> fail "Network.Delicious.JSON: Missing JSON field: top_tags"
                        Just (JSObject (JSONObject obj)) ->
                          liftM (reverse . sortBy (comparing snd)) $
                            mapM (\(x,y) -> readJSON y >>= \y' -> return (x,y')) obj
                        Just x -> 
			  fail ("Network.Delicious.JSON: Unexpected JSON value for 'top_tags': " ++ show x)

             the_total <- case lookup "total_posts" pairs of
                        Nothing -> fail "Network.Delicious.JSON: Missing JSON field: total_posts"
                        Just  n -> readJSON n

             the_url <- case lookup "url" pairs of
                         Nothing -> fail "Network.Delicious.JSON: Missing JSON field: url"
                         Just  n -> readJSON n

             hsh     <- readJSON (fromMaybe JSNull (lookup "hash" pairs))
             return $
                URLDetails { total = the_total
                           , url   = the_url
                           , tags  = the_tags
			   , hash  = hsh
			   }

    readJSON s = fail ("Network.Delicious.JSON: url details malformed: "++ show s)
