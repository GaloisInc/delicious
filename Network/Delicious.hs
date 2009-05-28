--------------------------------------------------------------------
-- |
-- Module    : Network.Delicious
-- Copyright : (c) Sigbjorn Finne, 2008-
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sigbjorn.finne@gmail.com>
-- Stability : provisional
-- Portability:
--
-- Binding to del.icio.us tagging system
--

module Network.Delicious
       ( module Network.Delicious.Types
       , module Network.Delicious.User
       , module Network.Delicious.JSON
       ) where

import Network.Delicious.Types
import Network.Delicious.User
-- default is JSON; selectively import the RSS one
-- if you want to use it instead.
import Network.Delicious.JSON
--import Network.Delicious.RSS

