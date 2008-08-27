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


module Network.Delicious.Types 
       ( DateString
       , TimeString
       , URLString

       , User(..)
       , nullUser
       
       , DM
       , withUser  -- :: User -> DM a -> DM a
       , withCount -- :: Int -> DM a -> DM a
       , getUser   -- :: DM User
       , getBase   -- :: DM URLString
       , getCount  -- :: DM (Maybe Int)

       , liftIO   -- :: IO a -> DM a
       , runDelic -- :: User -> URLString -> DM a -> IO a
       , runDM    -- :: User -> DM a -> IO a
       
       , Tag
       , TagInfo(..)
       , Bundle(..)

       , Filter(..)
       , nullFilter
       
       , Post(..)
       , nullPost
       
       ) where

import Web.DAV.Types ( URLString )

type DateString = String
type TimeString = String -- 8601

data DMEnv
 = DMEnv
     { dmUser  :: User
     , dmBase  :: URLString
     , dmCount :: Maybe Int
     }

data User
 = User
     { userName :: String
     , userPass :: String
     } deriving ( Show )

nullUser :: User
nullUser
 = User { userName = ""
        , userPass = ""
	}

newtype DM a = DM {unDM :: DMEnv -> IO a}

instance Monad DM where
  return x = DM $ \ _   -> return x
  m >>= k  = DM $ \ env -> do
     v <- unDM m env
     unDM (k v)  env

withUser :: User -> DM a -> DM a
withUser u k = DM $ \ env -> (unDM k) env{dmUser=u}

withCount :: Int -> DM a -> DM a
withCount c k = DM $ \ env -> (unDM k) env{dmCount=Just c}

getUser :: DM User
getUser = DM $ \ env -> return (dmUser env)

getCount :: DM (Maybe Int)
getCount = DM $ \ env -> return (dmCount env)

getBase :: DM URLString
getBase = DM $ \ env -> return (dmBase env)

liftIO :: IO a -> DM a
liftIO a = DM $ \ _ -> a

runDelic :: User -> URLString -> DM a -> IO a
runDelic u b dm = (unDM dm) DMEnv{dmUser=u,dmBase=b,dmCount=Nothing}

del_base :: URLString
del_base = "https://api.del.icio.us/v1"

runDM :: User -> DM a -> IO a
runDM user a = runDelic user del_base a

-- 

type Tag = String

data TagInfo
 = TagInfo
     { tagName :: Tag
     , tagUses :: Integer
     } deriving ( Show )

data Bundle
 = Bundle
     { bundleName :: String
     , bundleTags :: [Tag]
     } deriving ( Show )

data Filter
 = Filter
     { filterTag   :: Maybe Tag -- it looks as if no more than one can be given
     , filterDate  :: Maybe DateString
     , filterURL   :: Maybe URLString
     , filterCount :: Maybe Integer
     } deriving ( Show )

nullFilter :: Filter
nullFilter =
  Filter{ filterTag   = Nothing
        , filterDate  = Nothing
        , filterURL   = Nothing
        , filterCount = Nothing
        }

data Post
 = Post
     { postHref   :: URLString
     , postDesc   :: String
     , postNotes  :: String
     , postTags   :: [Tag]
     , postStamp  :: DateString
     , postHash   :: String
     } deriving ( Show )

nullPost :: Post
nullPost = Post
     { postHref   = ""
     , postDesc   = ""
     , postNotes  = ""
     , postTags   = []
     , postStamp  = ""
     , postHash   = ""
     }

