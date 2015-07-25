{-# LANGUAGE OverloadedStrings #-}

module Sachverhalt.Client.Types
    ( emptyRequest
    , dumpRequest
    , parseResponse
    ) where

import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Types (parseMaybe)
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Applicative ((<$>))
import Control.Lens

emptyRequest :: Value
emptyRequest = Object M.empty

dumpRequest :: Value -> B.ByteString
dumpRequest r = encode . object . M.toList $ r ^?! _Object

parseResponse :: B.ByteString -> Maybe Value
parseResponse str = str ^? _Value
