--------------------------------------------------------------------
-- |
-- Module      : Network.Delicious.JSON
-- Copyright   : (c) Galois, Inc. 2008
-- License     : BSD3
--
-- Maintainer  : Sigbjorn Finne <sigbjorn.finne@gmail.com>
-- Stability   : provisional
-- Portability : portable
--
-- Simple GET\/de-ref of URLs; abstracting out networking backend\/package.
--
module Network.Delicious.Fetch 
       ( readContentsURL
       , readUserContentsURL

       , URLString
       ) where
       
{-
 Note regarding HTTP libraries: the main reason for
 using Curl rather than HTTP is that del.icio.us uses
 HTTPS for parts of its API offerings. 
-}
import Network.Curl
import Network.Delicious.Types

-- | @readContentsURL@ fetches the content from the given URL, @u@.
-- Via a standard, non-authenticated, @GET@.
readContentsURL :: String -> URLString -> IO String
readContentsURL ua u = do
  let opts = [ CurlFollowLocation True
	     , CurlUserAgent ua
	     ]
  (_,xs) <- curlGetString u opts
  return xs

-- | Like 'readContentsURL', but HTTP authenticated using the supplied
-- credentials.
readUserContentsURL :: User -> String -> URLString -> IO String
readUserContentsURL u ua url = do
  let opts = [ CurlHttpAuth [HttpAuthAny]
             , CurlUserPwd (userName u ++ 
	                    case userPass u of {"" -> ""; p -> ':':p })
             , CurlFollowLocation True
	     , CurlUserAgent ua
	     ]
  (_,xs) <- curlGetString url opts
  return xs
     

