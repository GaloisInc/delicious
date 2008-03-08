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


module Network.Delicious.Types where

import Web.DAV.Types ( URLString )

type DateString = String
type TimeString = String -- 8601

data DMEnv
 = DMEnv
     { dmUser :: User
     , dmBase :: URLString
     }

data User
 = User
     { userName :: String
     , userPass :: String
     }

newtype DM a = DM {unDM :: DMEnv -> IO a}

instance Monad DM where
  return x = DM $ \ _   -> return x
  m >>= k  = DM $ \ env -> do
     v <- unDM m env
     unDM (k v)  env

withUser :: User -> DM a -> DM a
withUser u k = DM $ \ env -> (unDM k) env{dmUser=u}

getUser :: DM User
getUser = DM $ \ env -> return (dmUser env)

getBase :: DM URLString
getBase = DM $ \ env -> return (dmBase env)

liftIO :: IO a -> DM a
liftIO a = DM $ \ _ -> a

runDelic :: User -> URLString -> DM a -> IO a
runDelic u b dm = (unDM dm) DMEnv{dmUser=u,dmBase=b}

-- 

type Tag = String

data TagInfo
 = TagInfo
     { tagName :: Tag
     , tagUses :: Integer
     }

data Bundle
 = Bundle
     { bundleName :: String
     , bundleTags :: [Tag]
     }

data Filter
 = Filter
     { filterTag   :: Maybe Tag -- it looks as if no more than one can be given
     , filterDate  :: Maybe DateString
     , filterURL   :: Maybe URLString
     , filterCount :: Maybe Integer
     }

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
     }

nullPost :: Post
nullPost = Post
     { postHref   = ""
     , postDesc   = ""
     , postNotes  = ""
     , postTags   = []
     , postStamp  = ""
     , postHash   = ""
     }

