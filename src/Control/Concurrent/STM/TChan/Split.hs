------------------------------------------------------------------------------
-- |
-- Module:      Control.Concurrent.STM.TChan.Split.Internal
-- Copyright:   (c) 2013 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
------------------------------------------------------------------------------

module Control.Concurrent.STM.TChan.Split
    ( SendPort
    , ReceivePort
    , new
    , newSendPort
    , send
    , receive
    , tryReceive
    , peek
    , tryPeek
    , unget
    , isEmpty
    , listen
    , duplicate
    , split
    ) where

import Control.Concurrent.STM.TChan.Split.Implementation
