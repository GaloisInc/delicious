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
{-
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
       , getURLSummary          -- :: URLString -> IO [Post]
-}
       ) where

import Text.JSON
import Text.JSON.Types

import Network.Delicious.Types

import Control.Monad
import Data.List
import Data.Ord
import Data.Char
import Data.Maybe

import Data.Digest.OpenSSL.MD5
import qualified Data.ByteString.Char8 as S
import Web.DAV.Client.Curl

------------------------------------------------------------------------

-- | Retrieve tags associated with a url from delicious. 
-- An example, extract the tags associated with 'xmonad':
--
-- > > getURLDetails "http://xmonad.org/"
-- >
-- > Right (URLDetails {total = 283
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
getURLDetails :: String -> IO (Either String URLDetails)
getURLDetails url = do
    s <- readContentsURL final_url
    return $ case decodeStrict s of
        Ok s    -> Right s
        Error s -> Left s

  where final_url = baseUrl ++ (hashURL url)

baseUrl :: String
--baseUrl = "http://badges.del.icio.us/feeds/json/url/data?hash="
baseUrl = "http://feeds.delicious.com/v2/json"

hashURL url = md5sum (S.pack (clean url))
      where clean    = let f = reverse . dropWhile isSpace in f . f

------------------------------------------------------------------------

getHotlist :: IO [Post]
getHotlist = do
    s <- readContentsURL hot_url
    case decodeStrict s of
      Ok s    -> return s
      Error s -> ioError $ userError ("getHotlist: " ++ s)

  where hot_url = baseUrl

getRecentBookmarks :: IO [Post]
getRecentBookmarks = do
    s <- readContentsURL rec_url
    case decodeStrict s of
      Ok s    -> return s
      Error s -> ioError $ userError ("getRecent: " ++ s)

  where rec_url = baseUrl ++ "/recent"

getTagBookmarks :: Tag -> IO [Post]
getTagBookmarks tg = do
    s <- readContentsURL eff_url
    case decodeStrict s of
      Ok s    -> return s
      Error s -> ioError $ userError ("getTagBookmarks: " ++ s)

  where eff_url = baseUrl ++ "/tag/" ++ tg

getTagsBookmarks    :: [Tag] -> IO [Post]
getTagsBookmarks tgs = do
    s <- readContentsURL eff_url
    case decodeStrict s of
      Ok s    -> return s
      Error s -> ioError $ userError ("getTagsBookmarks: " ++ s)

  where eff_url = baseUrl ++ "/tag/" ++ concat (intersperse "+" tgs)

getPopularBookmarks :: IO [Post]
getPopularBookmarks = do
    s <- readContentsURL eff_url
    case decodeStrict s of
      Ok s    -> return s
      Error s -> ioError $ userError ("getPopularBookmarks: " ++ s)

  where eff_url = baseUrl ++ "/popular"

getTagPopularBookmarks :: Tag -> IO [Post]
getTagPopularBookmarks = undefined
getSiteAlerts       :: IO [Post]
getSiteAlerts = undefined
getUserBookmarks    :: String -> IO [Post]
getUserBookmarks = undefined
getUserTagBookmarks :: String -> Tag -> IO [Post]
getUserTagBookmarks = undefined
getUserTaggedBookmarks :: String -> [Tag] -> IO [Post]
getUserTaggedBookmarks = undefined
getUserInfo            :: String -> IO [Post]
getUserInfo = undefined
getUserPublicTags      :: String -> IO [Post]
getUserPublicTags = undefined
getUserSubscriptions   :: String -> IO [Post]
getUserSubscriptions = undefined
getUserInboxBookmarks  :: String -> String -> IO [Post]
getUserInboxBookmarks = undefined
getNetworkMemberBookmarks :: String -> IO [Post]
getNetworkMemberBookmarks = undefined
getNetworkMemberTaggedBookmarks :: String -> [Tag] -> IO [Post]
getNetworkMemberTaggedBookmarks = undefined
getNetworkMembers      :: String -> IO [Post]
getNetworkMembers = undefined
getNetworkFans         :: String -> IO [Post]
getNetworkFans = undefined
getURLBookmarks        :: URLString -> IO [Post]
getURLBookmarks = undefined
getURLSummary          :: URLString -> IO [Post]
getURLSummary = undefined

------------------------------------------------------------------------

-- | A structure represening the the delicious tags associated with a url.
data URLDetails =
        URLDetails { total :: !Integer
                   , tags  :: [(String,Integer)] }
        deriving (Eq,Show,Read)

-- | Compose and decompose URLDetails as JSON in the form delicious uses.
instance JSON URLDetails where
{-
    showJSON (URLDetails total tags) =
        JSArray . return . JSObject $ toJSObject
                    [ ("hash",            showJSON (JSONString (hashURL url)))
                    , ("total_posts",     showJSON total)
                    , ("top_tags",        JSObject
                                                (toJSObject
                                                    [(x,showJSON y) | (x,y) <- tags ]
                                                )
                      )
                   ]
-}

    readJSON (JSArray []) = return (URLDetails 0 [])
    readJSON (JSArray [JSObject (JSONObject pairs)])
        = do the_tags <- case lookup "top_tags" pairs of
                        Nothing -> fail "Network.Delicious.JSON: Missing JSON field: top_tags"
                        Just (JSObject (JSONObject obj)) ->
                          liftM (reverse . sortBy (comparing snd)) $
                            mapM (\(x,y) -> readJSON y >>= \y' -> return (x,y')) obj

             the_total <- case lookup "total_posts" pairs of
                        Nothing -> fail "Network.Delicious.JSON: Missing JSON field: total_posts"
                        Just  n -> readJSON n

         --  the_url <- case lookup "url" pairs of
         --             Nothing -> fail "Network.Delicious.JSON: Missing JSON field: url"
         --             Just  n -> readJSON n

             return $
                URLDetails { total = the_total
                           , tags  = the_tags }
                       --  , url   = the_url }

    readJSON s = fail ("Network.Delicious.JSON: url details malformed: "++ show s)

instance JSON Post where
--    showJSON p = ...

    readJSON (JSArray []) = return nullPost
    readJSON (JSObject (JSONObject pairs))
        = do tags <- case lookup "t" pairs of
                       Just n -> readJSON n
                       Nothing -> return []
             url <- case lookup "u" pairs of
                        Nothing -> fail "Network.Delicious.JSON: Missing JSON field: url"
                        Just  n -> readJSON n

             notes <- case lookup "n" pairs of
                        Nothing -> return ""
                        Just  n -> readJSON n
             desc <- case lookup "d" pairs of
                        Nothing -> return ""
                        Just  n -> readJSON n
             ts <- case lookup "dt" pairs of
                        Nothing -> return ""
                        Just  n -> readJSON n

             return $ nullPost{ postHref=url
	                      , postDesc=desc
			      , postNotes=notes
			      , postTags=tags
			      , postStamp=ts
			      }

    readJSON s = fail ("Network.Delicious.JSON: malformed post: "++ show s)


