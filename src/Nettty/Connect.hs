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

import           Network as N
import           System.IO
import           Data.Maybe
import           Network.BSD
import qualified Data.IntMap as M
import           Control.Monad
import           Nettty.Common
import           Network.Socket
import           Nettty.Protocol
import qualified Data.ByteString as B
import           Control.Concurrent
import           Network.Socket.Options
import           Control.Concurrent.STM

newtype Connections = Connections { conns :: TVar (M.IntMap Socket) }

start :: IO ()
start = do
  c <- fmap Connections (newTVarIO M.empty)
  B.hPut stdout (dump Ready)
  iothread c

sendmsg :: Message -> IO ()
sendmsg m@(Term _) = do
  notice slaveToken $ "sendmsg: " ++ show m
  B.hPut stdout (dump m)
sendmsg m          = B.hPut stdout (dump m)

iothread :: Connections -> IO ()
iothread c = superviseSentinel (fmap not (hIsEOF stdin)) $ do
  mchunk <- fmap load B.getLine
  when (isJust mchunk) (exec c (fromJust mchunk))

select :: Connections -> Channel -> STM (Maybe Socket)
select c k = fmap (M.lookup (ch k)) (readTVar (conns c))

destroy :: Connections -> Channel -> STM (Maybe Socket)
destroy c k = do
  m <- readTVar (conns c)
  writeTVar (conns c) (M.delete (ch k) m)
  return (M.lookup (ch k) m)

termQ :: Connections -> Channel -> IO Bool
termQ c chan = do
  mfh <- atomically $ destroy c chan
  when (isJust mfh) (N.sClose (fromJust mfh))
  return (isJust mfh)

term :: Connections -> Channel -> IO ()
term c chan = do
  ok <- termQ c chan
  when ok (sendmsg (Term chan))

tcpConnect :: Connections -> Channel -> String -> PortNumber -> IO ()
tcpConnect c chan host port = do
  proto <- getProtocolNumber "tcp"
  addr  <- getHostByName host
  sh    <- socket AF_INET Stream proto
  setLinger sh (Just 1)
  connect sh (SockAddrInet port (hostAddress addr))
  atomically $ modifyTVar (conns c) (M.insert (ch chan) sh)
  void $ forkFinally
    (copyWith (dump . Recv chan) sh stdout)
    (\_ -> term c chan)

exec :: Connections -> Message -> IO ()
exec c m@(Open chan (TCP host port))           =
  void $ forkIO $ do
    notice slaveToken $ "iothread: " ++ show m
    tcpConnect c chan host port
    sendmsg (Recv chan "done")
exec c m@(Open chan (HTTPConnect host port))   = do
  void $ forkIO $ do
    notice slaveToken $ "iothread: " ++ show m
    tcpConnect c chan host port
    sendmsg (Recv chan "HTTP/1.0 200 OK\r\n\r\n")
exec c m@(Term chan)                           = do
  void $ term c chan
  notice slaveToken $ "iothread: " ++ show m
exec c (Send chan msg)                         = do
  mfh <- atomically $ select c chan
  when (isJust mfh) (iowrite (fromJust mfh) msg)
exec _ _                                       = return ()
