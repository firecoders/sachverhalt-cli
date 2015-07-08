{-# LANGUAGE OverloadedStrings #-}

module Sachverhalt.Client.Types
    ( Request
    , emptyRequest
    , dumpRequest
    , requestInsert
    , requestLookup
    , Response
    , parseResponse
    , responseLookup
    ) where

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Applicative ((<$>))

type Request = Object
type Response = Object

emptyRequest :: Request
emptyRequest = M.empty

dumpRequest :: Request -> B.ByteString
dumpRequest = encode . object . M.toList

requestInsert :: (ToJSON a) => T.Text -> a -> Request -> Request
requestInsert key val = M.insert key (toJSON val)

requestLookup :: (FromJSON a) => T.Text -> Request -> Maybe a
requestLookup key req = M.lookup key req >>= parseMaybe parseJSON

parseResponse :: B.ByteString -> Maybe Response
parseResponse = decode

responseLookup :: (FromJSON a) => T.Text -> Response -> Maybe a
responseLookup key resp = M.lookup key resp >>= parseMaybe parseJSON
