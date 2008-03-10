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

module Network.Delicious.JSON where

import Text.JSON
import Text.JSON.Types
import Control.Monad
import Data.List
import Data.Ord
import Data.Char
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
baseUrl = "http://badges.del.icio.us/feeds/json/url/data?hash="

hashURL url = md5sum (S.pack (clean url))
      where clean    = let f = reverse . dropWhile isSpace in f . f

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
