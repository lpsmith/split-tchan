{-# LANGUAGE DeriveDataTypeable #-}

------------------------------------------------------------------------------
-- |
-- Module:      Control.Concurrent.STM.TChan.Split.Implementation
-- Copyright:   (c) 2013 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
------------------------------------------------------------------------------


module Control.Concurrent.STM.TChan.Split.Implementation where

import Control.Concurrent.STM

import Data.Typeable (Typeable)

type TVarList a = TVar (TList a)
data TList a = TNil | TCons a {-# UNPACK #-} !(TVarList a)

newtype SendPort a 
    = SendPort (TVar (TVarList a)) 
      deriving (Eq, Typeable)

newtype ReceivePort a 
    = ReceivePort (TVar (TVarList a))
      deriving (Eq, Typeable)

new :: STM (SendPort a, ReceivePort a)
new = do
    hole  <- newTVar TNil
    read  <- newTVar hole
    write <- newTVar hole
    return (SendPort write, ReceivePort read)

newSendPort :: STM (SendPort a)
newSendPort = do
    hole  <- newTVar TNil
    write <- newTVar hole
    return (SendPort write)

send :: SendPort a -> a -> STM ()
send (SendPort write) a = do
    listend <- readTVar write
    new_listend <- newTVar TNil
    writeTVar listend (TCons a new_listend)
    writeTVar write new_listend

receive :: ReceivePort a -> STM a
receive (ReceivePort read) = do
    listhead <- readTVar read
    head <- readTVar listhead
    case head of
      TNil -> retry
      TCons a tail -> do
          writeTVar read tail
          return a

tryReceive :: ReceivePort a -> STM (Maybe a)
tryReceive (ReceivePort read) = do
    listhead <- readTVar read
    head <- readTVar listhead
    case head of
      TNil -> return Nothing
      TCons a tail -> do
          writeTVar read tail
          return (Just a)

peek :: ReceivePort a -> STM a
peek (ReceivePort read) = do
    listhead <- readTVar read
    head <- readTVar listhead
    case head of
      TNil -> retry
      TCons a _tail -> do
          return a

tryPeek :: ReceivePort a -> STM (Maybe a)
tryPeek (ReceivePort read) = do
    listhead <- readTVar read
    head <- readTVar listhead
    case head of
      TNil -> return Nothing
      TCons a _tail -> do
         return (Just a)

unget :: ReceivePort a -> a -> STM ()
unget (ReceivePort read) a = do
    listhead <- readTVar read
    new_listhead <- newTVar $! TCons a listhead
    writeTVar read new_listhead

isEmpty :: ReceivePort a -> STM Bool
isEmpty (ReceivePort read) = do
    listhead <- readTVar read
    head <- readTVar listhead
    case head of
      TNil      -> return True
      TCons _ _ -> return False

listen :: SendPort a -> STM (ReceivePort a)
listen (SendPort write) = do
    listend <- readTVar write
    read <- newTVar listend
    return (ReceivePort read)

duplicate :: ReceivePort a -> STM (ReceivePort a)
duplicate (ReceivePort read) = do
    listhead <- readTVar read
    read <- newTVar listhead
    return (ReceivePort read)  

split :: SendPort a -> STM (ReceivePort a, SendPort a)
split (SendPort write) = do
    new_hole <- newTVar TNil
    old_hole <- swapTVar write new_hole
    read     <- newTVar new_hole
    write'   <- newTVar old_hole
    return (ReceivePort read, SendPort write') 
    
