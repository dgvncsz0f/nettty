{-# LANGUAGE OverloadedStrings #-}

-- Copyright (c) 2013, Diego Souza
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
--   * Redistributions of source code must retain the above copyright notice,
--     this list of conditions and the following disclaimer.
--   * Redistributions in binary form must reproduce the above copyright notice,
--     this list of conditions and the following disclaimer in the documentation
--     and/or other materials provided with the distribution.
--   * Neither the name of the <ORGANIZATION> nor the names of its contributors
--     may be used to endorse or promote products derived from this software
--     without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE

module Nettty.Connect
       ( Connections
       , start
       ) where

import           Network
import           System.IO
import           Data.Maybe
import           Network.BSD
import qualified Data.IntMap as M
import           Control.Monad
import           Nettty.Common
import           Network.Socket
import           Nettty.Protocol
import qualified Data.ByteString as B
import           Control.Exception
import           Control.Concurrent
import           Network.Socket.Options
import           Control.Concurrent.STM

newtype Connections = Connections { conns :: TVar (M.IntMap (Socket, Int)) }

start :: IO ()
start = do
  c <- fmap Connections (newTVarIO M.empty)
  _ <- forkIO (supervise $ rungc c >> sleep 1)
  B.hPut stdout (dump Ready)
  iothread c

expired :: (Socket, Int) -> Bool
expired (_, t) = t < 0

defTTL :: Int
defTTL = 60

setTTL :: Int -> (Socket, Int) -> (Socket, Int)
setTTL t (h, _) = (h, t)

decTTL :: (Socket, Int) -> (Socket, Int)
decTTL (h, t) = (h, t-1)

expire :: Connections -> IO [Int]
expire c = atomically $ do
  (dead, alive) <- fmap (M.partition expired) (readTVar (conns c))
  writeTVar (conns c) (M.map decTTL alive)
  return (M.keys dead)

rungc :: Connections -> IO ()
rungc c = expire c >>= mapM_ (purge . Channel)
    where purge chan = do
            let m = Term chan
            exec c m
            sendmsg m

sendmsg :: Message -> IO ()
sendmsg m@(Term _) = do
  debugMany [slaveToken, "sendmsg: ", show m]
  B.hPut stdout (dump m)
sendmsg m          = B.hPut stdout (dump m)

iothread :: Connections -> IO ()
iothread c = supervise $ do
  mchunk <- fmap load B.getLine
  when (isJust mchunk) (exec c (fromJust mchunk))

select :: Connections -> Channel -> STM (Maybe Socket)
select c k = do
  m <- readTVar (conns c)
  writeTVar (conns c) (M.alter (fmap (setTTL defTTL)) (ch k) m)
  return (fmap fst $ M.lookup (ch k) m)

destroy :: Connections -> Channel -> STM (Maybe Socket)
destroy c k = do
  m <- readTVar (conns c)
  writeTVar (conns c) (M.delete (ch k) m)
  return (fmap fst $ M.lookup (ch k) m)

term :: Connections -> Channel -> IO Bool
term c chan = do
  mfh <- atomically $ destroy c chan
  when (isJust mfh) (ioclose (fromJust mfh))
  return (isJust mfh)

exec :: Connections -> Message -> IO ()
exec c m@(Open chan (EndpointTCP host port)) = do
  debugMany [slaveToken, "iothread: ", show m]
  proto <- getProtocolNumber "tcp"
  sh    <- bracketOnError
             (socket AF_INET Stream proto)
             sClose
             (\sh -> do
                 setLinger sh (Just 1)
                 addr <- getHostByName host
                 connect sh (SockAddrInet port (hostAddress addr))
                 return sh)
  _  <- forkIO (copyWith (dump . Recv chan) sh stdout `catch` ignore)
  atomically $ modifyTVar (conns c) (M.insert (ch chan) (sh, defTTL))
  return ()
exec c m@(Term chan)          = do
  _ <- term c chan
  debugMany [slaveToken, "iothread: ", show m]
exec c (Send chan msg)       = do
  mfh <- atomically $ select c chan
  when (isJust mfh) (iowrite (fromJust mfh) msg)
exec _ _                     = return ()
