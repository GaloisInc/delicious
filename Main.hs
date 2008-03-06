module Main where

import Network.Delicious
import System.Environment
import System.Exit
import Control.Monad ( when )

base = "https://api.del.icio.us/v1"

main :: IO ()
main = do
  ls <- getArgs
  when (length ls /= 2) $ do
       putStrLn "Usage: dtest <del.icio.us username> <password>"
       exitFailure
  let (x:y:_) = ls
  let usr = User{userName=x,userPass=y}
  v <- runDelic usr base getTags
  print (map tagName v)
  v <- runDelic usr base getBundles
  mapM_ (\ b -> putStrLn (bundleName b ++ " = " ++ show (bundleTags b))) v
  ps <- runDelic usr base (getPosts nullFilter)
  mapM_ (\ p -> putStrLn (postHref p  ++ " : " ++ show (postTags p))) ps
  ps <- runDelic usr base (getRecent Nothing (Just 100))
  mapM_ (\ p -> putStrLn (postHref p  ++ " : " ++ show (postTags p))) ps
  ps <- getRSSTag "haskell"
  mapM_ (\ p -> putStrLn (postHref p  ++ " : " ++ postDesc p ++ " : " ++ show (postTags p))) ps

