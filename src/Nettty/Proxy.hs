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

module Nettty.Proxy
       ( Proxy
       , nettty
       , open
       , term
       , sendmsg
       , recvmsg
       , wait
       , kill
       , proxyFrom
       , proxyTo
       , proc
       ) where

import           System.IO
import           Data.Maybe
import qualified Data.IntMap as M
import           Nettty.Common
import           Control.Monad
import           System.Process
import           Nettty.Protocol
import qualified Data.ByteString as B
import           Control.Exception
import           Control.Concurrent
import           Control.Concurrent.STM

data Proxy = Proxy { pSeq :: TVar Int
                   , pInq :: TVar (M.IntMap (TBQueue B.ByteString))
                   , pIfh :: Handle
                   , pPfh :: ProcessHandle
                   }

select :: Channel -> TVar (M.IntMap a) -> STM (Maybe a)
select chan = fmap (M.lookup (ch chan)) . readTVar

destroy :: Channel -> TVar (M.IntMap a) -> STM Bool
destroy chan tvar = do
  ok <- fmap (M.member (ch chan)) (readTVar tvar)
  when ok (modifyTVar' tvar (M.delete (ch chan)))
  return ok

iothread :: Handle -> Proxy -> IO ()
iothread h p = forkIO (copy stdin (pIfh p)) >>= stg1
    where stg1 t = do
            line <- B.hGetLine h
            case (load line) of
              Nothing
                -> do B.hPut stdout line
                      hFlush stdout
                      stg1 t
              Just Ready
                -> do debugMany [masterToken, show Ready]
                      killThread t
                      hClose stdin
                      stg2
              _ -> error "what?"

          enqueue (Term chan)   = do
            debugMany [masterToken, "iothread: ", show (Term chan)]
            _ <- termQ p chan
            return ()
          enqueue (Recv chan m) = atomically $ do
            mq <- select chan (pInq p)
            when (isJust mq) (writeTBQueue (fromJust mq) m)
          enqueue _             = error "wat?"

          stg2 = supervise $ do
            line <- B.hGetLine h
            case (load line) of
              Nothing  -> debugMany [masterToken, "error parsing line", show line]
              Just chk -> enqueue chk

fromProcess :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO Proxy
fromProcess (Just ifh, Just ofh, _, pfh) = do
  hSetBuffering ifh NoBuffering
  hSetBuffering ofh NoBuffering
  hSetBinaryMode ifh True
  hSetBinaryMode ofh True
  inqueue <- newTVarIO M.empty
  seqgen  <- newTVarIO 0
  let p = Proxy seqgen inqueue ifh pfh
  _       <- forkIO (iothread ofh p)
  return p
fromProcess _                            = fail "cant spawn process"

nettty :: CreateProcess -> IO Proxy
nettty cmd = do
  rawp <- createProcess (cmd { std_in    = CreatePipe
                             , std_out   = CreatePipe
                             , std_err   = Inherit
                             , close_fds = True
                             })
  fromProcess rawp

nextchan :: Proxy -> IO Channel
nextchan p = atomically $ do
  k <- readTVar (pSeq p)
  q <- newTBQueue 64
  writeTVar (pSeq p) (k + 1)
  modifyTVar' (pInq p) (M.insert k q)
  return (Channel k)

send :: Proxy -> Message -> IO ()
send p m@(Term _) = do
  debugMany [masterToken, "send: ", show m]
  B.hPut (pIfh p) (dump m)
send p m          = B.hPut (pIfh p) (dump m)

sendmsg :: Proxy -> Channel -> B.ByteString -> IO ()
sendmsg p chan msg = send p (Send chan msg)

recvmsg :: Proxy -> Channel -> IO B.ByteString
recvmsg p chan = atomically $ do 
  mq <- select chan (pInq p)
  case mq of
    Nothing -> return B.empty
    Just q  -> readTBQueue q

open :: Proxy -> Endpoint -> IO Channel
open p endpoint = do
  chan <- nextchan p
  debugMany [masterToken, show (Open chan endpoint)]
  send p (Open chan endpoint)
  return chan

term :: Proxy -> Channel -> IO ()
term p chan = do
  ok <- termQ p chan
  when ok (send p (Term chan))

termQ :: Proxy -> Channel -> IO Bool
termQ p chan = atomically $ destroy chan (pInq p)

wait :: Proxy -> IO ()
wait p = waitForProcess (pPfh p) >> return ()

kill :: Proxy -> IO ()
kill p = terminateProcess (pPfh p)

proxyFrom :: Handle -> Proxy -> Channel -> IO ()
proxyFrom h p c = (copyTo h $ sendmsg p c) `finally` term p c

proxyTo :: Proxy -> Channel -> Handle -> IO ()
proxyTo p c h = copyFrom (recvmsg p c) h
