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

module Nettty.Common where

import           Network
import           System.IO
import           Control.Monad
import qualified Data.ByteString as B
import           Control.Exception
import           Control.Concurrent
import           Control.Concurrent.STM
import           Network.Socket.ByteString

class HasIO a where

  iowrite :: a -> B.ByteString -> IO ()
  ioread  :: a -> Int -> IO B.ByteString
  ioclose :: a -> IO ()

masterToken :: String
masterToken = "proxy /"

slaveToken :: String
slaveToken = "nettty/"

copy :: (HasIO fh0, HasIO fh1) => fh0 -> fh1 -> IO ()
copy = copyWith id

copyWith :: (HasIO fh0, HasIO fh1) => (B.ByteString -> B.ByteString) -> fh0 -> fh1 -> IO ()
copyWith f src dst = copyTo src (iowrite dst . f)

copyTo :: HasIO fh => fh -> (B.ByteString -> IO ()) -> IO ()
copyTo fh sink = bracket (return fh) ioclose ioloop
    where ioloop src = do
            msg <- ioread src 512
            when (not $ B.null msg) (sink msg >> ioloop src)

copyFrom :: HasIO fh => (IO B.ByteString) -> fh -> IO ()
copyFrom source fh = bracket (return fh) ioclose ioloop
    where ioloop dst = do
            msg <- source
            when (not $ B.null msg) (iowrite dst msg >> ioloop dst)

signal :: TMVar () -> IO ()
signal = atomically . flip putTMVar ()

consume :: TMVar () -> IO ()
consume = atomically . takeTMVar

forkWait :: IO () -> IO ()
forkWait io = do
  lock <- newEmptyTMVarIO
  _    <- forkFinally io (\_ -> signal lock)
  consume lock

forkWait2 :: IO () -> IO () -> IO ()
forkWait2 io1 io2 = do
  lock <- newEmptyTMVarIO
  _    <- forkFinally io1 (\_ -> signal lock)
  _    <- forkFinally io2 (\_ -> signal lock)
  replicateM_ 2 (consume lock)

sleep :: Int -> IO ()
sleep s = threadDelay (s * 1000 * 1000)

ignore :: SomeException -> IO ()
ignore _ = return ()

supervise :: IO () -> IO ()
supervise io = mask $ \restore -> do
  restore io `catch` ignore
  supervise io

superviseSentinel :: IO Bool -> IO () -> IO ()
superviseSentinel test io = mask $ \restore -> do
  restore io `catch` ignore
  test >>= flip when (superviseSentinel test io)

debug :: String -> String -> IO ()
debug f m = hPutStr stderr ("D" ++ f ++ ": ") >> hPutStrLn stderr (filter (/='\n') m)

notice :: String -> String -> IO ()
notice f m = hPutStr stderr ("N" ++ f ++ ": ")  >> hPutStrLn stderr (filter (/='\n') m)

warning :: String -> String -> IO ()
warning f m = hPutStr stderr ("W" ++ f ++ ": ") >> hPutStrLn stderr (filter (/='\n') m)

instance HasIO Handle where

  ioread  = B.hGetSome
  iowrite = B.hPut
  ioclose = hClose

instance HasIO Socket where

  ioread  = recv
  iowrite = sendAll
  ioclose = sClose
