--------------------------------------------------------------------
-- |
-- Module      : Network.Delicious.User
-- Copyright   : (c) Galois, Inc. 2008
-- License     : BSD3
--
-- Maintainer  : Sigbjorn Finne <sigbjorn.finne@gmail.com>
-- Stability   : provisional
-- Portability : portable
--
-- Accessing a user's tags and bookmarks
--
--------------------------------------------------------------------
module Network.Delicious.User
       ( getLastUpdate   -- :: DM TimeString

       , getTags         -- :: DM [TagInfo]
       , renameTag       -- :: Tag -> Tag -> DM ()
       , deleteTag       -- :: Tag -> DM ()
       , getPosts        -- :: Filter -> DM [Post]

       , getRecent       -- :: Maybe Tag -> Maybe Integer -> DM [Post]
       , getAll          -- :: Maybe Tag -> DM [Post]
       , getAllHashes    -- :: DM [Post]
       , getByDate       -- :: Maybe Tag -> DM [(DateString,Integer)]

       , addPost         -- :: Post -> Bool -> Bool -> DM ()
       , deletePost      -- :: URLString -> DM ()

       , getBundles      -- :: DM [Bundle]
       , setBundle       -- :: String -> [Tag] -> DM ()
       , deleteBundle    -- :: String -> IO ()

       , restReq
       ) where

import Network.Delicious.Types
import Network.Delicious.Fetch


import Control.Monad
import Data.List
import Data.Maybe

import Text.XML.Light as XML hiding ( findAttr )

--
restReq :: String -> [(String,String)] -> DM (Either XML.Element String)
restReq cmd opts = do
  b      <- getBase
  u      <- getUser
  let effUrl = b ++ '/':cmd ++ tlOpts opts
  ua <- getUAgent
  xs <- liftIO $ readUserContentsURL u ua effUrl
  return (fromMaybe (Right xs) $ fmap Left $ parseXMLDoc xs)
 where
  tlOpts [] = ""
  tlOpts xs = '?':concat (intersperse "&" (map (\ (x,y) -> x ++ '=':y) xs))

--

getLastUpdate :: DM TimeString
getLastUpdate = do
  pl <- restReq "posts/update" []
  case pl of
    Right x -> fail ("getLastUpdate: no parse -- " ++ x)
    Left d ->
     case find (\ (Attr a _) -> qName a == "time") (elAttribs d) of
       Just (Attr _ v) -> return v
       Nothing    -> fail (show pl)

getTags   :: DM [TagInfo]
getTags = do
  pl <- restReq "tags/get" []
  case pl of
    Right x -> fail ("getTags: no parse -- " ++ x)
    Left d ->
     case qName $ elName d of
       "tags" -> return (map eltToTag $ findElements (unqual "tag") d)
       _ -> fail ("getTags: unexpected return payload " ++ show d)
 where
  eltToTag e =
    TagInfo
      { tagName = findAttr "tag" "" e
      , tagUses = readInt 0 $ findAttr "count" "0" e
      }

readInt :: Integer -> String -> Integer
readInt d xs =
  case reads xs of
    ((x,_):_) -> x
    _ -> d

findAttr :: String -> String -> Element -> String
findAttr n d e =
  fromMaybe d $
   fmap (\ (Attr _ v) -> v) $
    find (\ (Attr a _) -> qName a == n) (elAttribs e)

renameTag :: Tag -> Tag -> DM ()
renameTag ot nt = do
  pl <- restReq "tags/rename" [("old",ot),("new", nt)]
  case pl of
    Right x  -> fail ("renameTag: ill-formed return value -- " ++ x)
    Left d ->
     case qName $ elName d of
       "result" | strContent d == "done" -> return ()
       _ -> fail ("renameTag: unexpected return value " ++ show d)

deleteTag :: Tag -> DM ()
deleteTag dt = do
  pl <- restReq "tags/delete" [("tag",dt)]
  case pl of
    Right x -> fail ("deleteTag: ill-formed return value -- " ++ x)
    Left d  ->
     case qName $ elName d of
       "result" | strContent d == "done" -> return ()
       _ -> fail ("deleteTag: unexpected return value " ++ show d)


getPosts :: Filter -> DM [Post]
getPosts f = getPosts' "getPosts" "posts/get" f

getPosts' :: String -> String -> Filter -> DM [Post]
getPosts' loc r f = do
  pl <- restReq r (toFilterArgs f)
  case pl of
    Right x -> fail (loc ++ ": ill-formed return value -- " ++ x)
    Left d ->
       case qName $ elName d of
         "posts" -> return (map eltToPost $ findElements (unqual "post") d)
         _ -> fail (loc ++ ": unexpected return payload " ++ show d)
 where
   eltToPost e = Post
     { postHref   = findAttr "href" "" e
     , postDesc   = findAttr "description" "" e
     , postUser   = findAttr "user" "" e
     , postNotes  = findAttr "extended" "" e
     , postTags   = words $ findAttr "tag" "" e
     , postStamp  = findAttr "time" "" e
     , postHash   = findAttr "hash" "" e
     }

toFilterArgs :: Filter -> [(String,String)]
toFilterArgs f =
  mb "tag" (filterTag f) $
   mb "dt" (filterDate f) $
    mb "url" (filterURL f) []

mb :: a -> Maybe b -> [(a,b)] -> [(a,b)]
mb _ Nothing  xs = xs
mb t (Just v) xs = (t,v):xs

getRecent  :: Maybe Tag -> Maybe Integer -> DM [Post]
getRecent mbTg mbCount =
  getPosts' "getRecent" "posts/recent"
            nullFilter{filterTag=mbTg,filterCount=mbCount}

getAll :: Maybe Tag -> DM [Post]
getAll mbTg = getPosts' "getAll" "posts/all" nullFilter{filterTag=mbTg}

getAllHashes :: DM [Post]
getAllHashes = getPosts' "getAll" "posts/all?hashes" nullFilter

getByDate  :: Maybe Tag -> DM [(DateString,Integer)]
getByDate mbTg = do
  pl <- restReq "posts/dates" (toFilterArgs nullFilter{filterTag=mbTg})
  case pl of
    Right x -> fail ("getByDate: no parse -- " ++ x)
    Left d ->
     case qName $ elName d of
       "dates" -> return (map eltToDate $ findElements (unqual "date") d)
       _ -> fail ("getByDate: unexpected return payload " ++ show d)
 where
  eltToDate e =
    ( findAttr "date" "" e
    , readInt 0 $ findAttr "count" "0" e
    )

addPost :: Post -> Bool -> Bool -> DM ()
addPost ps replace shared = do
  pl <- restReq "posts/add" (toPostArgs ps)
  case pl of
    Right x -> fail ("addPost: ill-formed return value -- " ++ x)
    Left d ->
     case qName $ elName d of
       "result"
         | findAttr "code" "" d == "done" -> return ()
       _ -> fail ("addPost: unexpected return payload " ++ show d)
 where
  toPostArgs p =
     mb "url" (l2m $ postHref p) $
     mb "description" (l2m $ postDesc p) $
     mb "extended" (l2m $ postNotes p) $
     mb "tags" (l2m $ unwords $ postTags p) $
     mb "dt"  (l2m $ postStamp p) $
     mb "replace" (if replace then Just "yes" else Just "no") $
     mb "shared" (if shared then Just "yes" else Just "no")
        []
  l2m "" = Nothing
  l2m xs = Just xs

deletePost :: URLString -> DM ()
deletePost u = do
  pl <- restReq "posts/delete" [("url", u)]
  case pl of
    Right x -> fail ("deletePost: ill-formed return value -- " ++ x)
    Left d -> 
     case qName $ elName d of
       "result" 
         | findAttr "code" "" d == "done" -> return ()
       _ -> fail ("deletePost: unexpected return payload " ++ show d)

getBundles :: DM [Bundle]
getBundles = do
  pl <- restReq "tags/bundles/all" []
  case pl of
    Right x -> fail ("getBundles: ill-formed return value -- " ++ x)
    Left d ->
     case qName $ elName d of
       "bundles" -> return (map eltToBundle $ findElements (unqual "bundle") d)
       _ -> fail ("getBundles: unexpected return payload " ++ show d)
 where
  eltToBundle e = 
    Bundle
      { bundleName = findAttr "name" "" e
      , bundleTags = words $ findAttr "tag" "" e
      }

setBundle :: String -> [Tag] -> DM ()
setBundle nm tgs = do
  pl <- restReq "tags/bundles/set" [("bundle",nm),("tags", unwords tgs)]  
  case pl of
    Right x  -> fail ("setBundle: ill-formed return value -- " ++ x)
    Left d -> 
     case qName $ elName d of
       "result" | strContent d == "ok" -> return ()
       _ -> fail ("setBundle: unexpected return value " ++ show d)

deleteBundle :: String -> DM ()
deleteBundle nm = do
  pl <- restReq "tags/bundles/delete" [("bundle",nm)]
  case pl of
    Right x  -> fail ("deleteBundle: ill-formed return value -- " ++ x)
    Left d -> 
     case qName $ elName d of
       "result" | strContent d == "done" -> return ()
       _ -> fail ("deleteBundle: unexpected return value " ++ show d)

