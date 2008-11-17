--------------------------------------------------------------------
-- |
-- Module      : Network.Delicious.JSON
-- Copyright   : (c) Galois, Inc. 2008
-- License     : BSD3
--
-- Maintainer  : Sigbjorn Finne <sof@galois.com>
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
readContentsURL :: URLString -> IO String
readContentsURL u = do
  let opts = [ CurlFollowLocation True
	     ]
  (_,xs) <- curlGetString u opts
  return xs

-- | Like 'readContentsURL', but HTTP authenticated using the supplied
-- credentials.
readUserContentsURL :: User -> URLString -> IO String
readUserContentsURL u url = do
  let opts = [ CurlHttpAuth [HttpAuthAny]
             , CurlUserPwd (userName u ++ 
	                    case userPass u of {"" -> ""; p -> ':':p })
             , CurlFollowLocation True
	     ]
  (_,xs) <- curlGetString url opts
  return xs
     

