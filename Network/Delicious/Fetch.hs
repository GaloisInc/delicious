--------------------------------------------------------------------
-- |
-- Module    : Network.Delicious.JSON
-- Copyright : (c) Galois, Inc. 2008
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@galois.com>
-- Stability : provisional
-- Portability:
--
-- Simple GET\/de-ref of URLs; abstracting out networking backend\/package.
--
module Network.Delicious.Fetch 
       ( readContentsURL
       , readUserContentsURL

       , URLString
       ) where
       
import Network.Curl
import Network.Delicious.Types

readContentsURL :: URLString -> IO String
readContentsURL u = do
  let opts = [ CurlFollowLocation True
	     ]
  (_,xs) <- curlGetString u opts
  return xs

readUserContentsURL :: User -> URLString -> IO String
readUserContentsURL u url = do
  let opts = [ CurlHttpAuth [HttpAuthAny]
             , CurlUserPwd (userName u ++ 
	                    case userPass u of {"" -> ""; p -> ':':p })
             , CurlFollowLocation True
	     ]
  (_,xs) <- curlGetString url opts
  return xs
     

