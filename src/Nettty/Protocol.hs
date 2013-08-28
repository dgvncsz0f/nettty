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

module Nettty.Protocol
       ( Message (..)
       , Channel (..)
       , Endpoint (..)
       , pEndpoint
       , pMessage
       , bMessage
       , loadE
       , load
       , dump
       ) where

import           Data.Monoid
import           Network.Socket (PortNumber ())
import           Data.Attoparsec as P
import qualified Data.ByteString as B
import           Control.Applicative
import           Data.ByteString.Lazy (toStrict)
import           Data.Attoparsec.Char8
import           Data.ByteString.Char8 (unpack)
import           Data.ByteString.Base64
import           Data.ByteString.Lazy.Builder

magic :: B.ByteString
magic = "# -> nettty/"

newtype Channel = Channel { ch :: Int }

data Endpoint = EndpointTCP String PortNumber

data Message = Open Channel Endpoint
             | Send Channel B.ByteString
             | Recv Channel B.ByteString
             | Term Channel
             | Ready

pChan :: Parser Channel
pChan = fmap Channel decimal

pEndpoint :: Parser Endpoint
pEndpoint = do
  _    <- string "tcp://"
  host <- fmap unpack (P.takeWhile (/= 0x3a))
  _    <- P.word8 0x3a
  port <- decimal
  return (EndpointTCP host port)

pOpen :: Parser Message
pOpen = do
  chan <- pChan
  _    <- P.word8 0x20
  fmap (Open chan) pEndpoint

pRecv :: Parser Message
pRecv = do
  chan <- pChan
  _    <- P.word8 0x20
  body <- P.takeWhile (/= 0x0a) >>= either fail return . decode
  return (Recv chan body)

pSend :: Parser Message
pSend = do
  chan <- pChan
  _    <- P.word8 0x20
  body <- P.takeWhile (/= 0x0a) >>= either fail return . decode
  return (Send chan body)

pTerm :: Parser Message
pTerm = do
  chan <- pChan
  return (Term chan)

pMessage :: Parser Message
pMessage = do
  _ <- string magic
  m <- (   "open " .*> pOpen
       <|> "send " .*> pSend
       <|> "recv " .*> pRecv
       <|> "term " .*> pTerm
       <|> "ready" .*> return Ready)
  _ <- P.word8 0x0a
  return m

bChannel :: Channel -> Builder
bChannel chan = string7 (show $ ch chan)

bEndpoint :: Endpoint -> Builder
bEndpoint (EndpointTCP host port) =
  (  string7 "tcp://" 
  <> string7 host
  <> char7 ':'
  <> string7 (show $ toInteger port))

dump :: Message -> B.ByteString
dump = toStrict . toLazyByteString . bMessage

load :: B.ByteString -> Maybe Message
load m0 = either (const Nothing) Just (parseOnly pMessage m1)
    where m1 = if ("\n" `B.isSuffixOf` m0)
                 then m0
                 else m0 `B.append` "\n"

loadE :: B.ByteString -> Maybe Endpoint
loadE = either (const Nothing) Just . parseOnly pEndpoint

bMessage :: Message -> Builder
bMessage (Open chan endpoint) = 
  (  byteString magic
  <> string7 "open "
  <> bChannel chan
  <> char7 ' '
  <> bEndpoint endpoint
  <> char7 '\n')
bMessage (Send chan bytes)     =
  (  byteString magic
  <> string7 "send "
  <> bChannel chan
  <> char7 ' '
  <> byteString (encode bytes)
  <> char7 '\n')
bMessage (Recv chan bytes)     =
  (  byteString magic
  <> string7 "recv "
  <> bChannel chan
  <> char7 ' '
  <> byteString (encode bytes)
  <> char7 '\n')
bMessage (Term chan)           =
  (  byteString magic
  <> string7 "term "
  <> bChannel chan
  <> char7 '\n')
bMessage Ready                 =
  (  byteString magic
  <> string7 "ready\n")

instance Show Message where

  show = unpack . dump

instance Show Channel where

  show = unpack . toStrict . toLazyByteString . bChannel

instance Show Endpoint where

  show = unpack . toStrict . toLazyByteString . bEndpoint
