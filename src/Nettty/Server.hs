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

module Nettty.Server
       ( start
       ) where

import           Network
import           System.IO
import           Nettty.Proxy
import           Nettty.Common
import           Control.Monad
import           Nettty.Protocol
import qualified Data.ByteString as B
import           Control.Exception
import           Control.Concurrent

readUntilEmpty :: Handle -> IO ()
readUntilEmpty fh = do
  line <- B.hGetLine fh
  when (line /= "\r" && line /= "") (readUntilEmpty fh)

ioloop :: Proxy -> Handle -> IO ()
ioloop p fh = do
  header <- B.hGetLine fh
  case (loadE header) of
    Nothing -> B.hPut fh "ERROR: bad endpoint"
    Just e  -> do
      when (isHttpConnect e) (readUntilEmpty fh)
      chan <- open p e
      forkWait2 (proxyFrom fh p chan) (proxyTo p chan fh)

start :: Proxy -> PortID -> IO ()
start p port = do
  s <- listenOn port
  forever $ do
    (fh, _, _) <- accept s
    hSetBuffering fh NoBuffering
    hSetBinaryMode fh True
    forkFinally (ioloop p fh) (\_ -> hClose fh)
