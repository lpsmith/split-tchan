{-# OPTIONS_HADDOCK hide #-}

------------------------------------------------------------------------------
-- |
-- Module:      Control.Concurrent.STM.TChan.Split.Internal
-- Copyright:   (c) 2013 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
------------------------------------------------------------------------------

module Control.Concurrent.STM.TChan.Split.Internal
    ( TVarList
    , TList
    , SendPort(..)
    , ReceivePort(..)
    ) where

import Control.Concurrent.STM.TChan.Split.Implementation
