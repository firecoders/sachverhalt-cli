{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Sachverhalt.Client.Monads
    ( Registration(..)
    , catRegistrations
    , register
    , RequestM
    , evalRequestM
    --, getReq
    , ResponseM
    , evalResponseM
    --, getResp
    , key'
    ) where

import Algorithms.Dependant (Dependant(..), DependantPlus(..), resolvePlus)
import Data.Aeson (FromJSON, ToJSON, Value, fromJSON, toJSON)
import Data.Aeson.Types (parseMaybe, parseJSON)
import Data.Aeson.Lens
import qualified Data.HashMap.Strict as M
import Control.Lens
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.State (StateT, runStateT, modify, gets)
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Applicative (pure)
import qualified Data.Text as T
import Sachverhalt.Client.Types (emptyRequest)

data Registration m = Registration
    { rIdentifier :: T.Text
    , rBefore :: [T.Text]
    , rBehind :: [T.Text]
    , registration :: m ()
    }

instance Show (Registration m) where
    show reg = show (rBehind reg) ++ " <- " ++ show (rIdentifier reg) ++ " -> " ++ show (rBefore reg)

instance (Dependant T.Text) (Registration m) where
    identifier = rIdentifier
    dependencies = rBehind

instance (DependantPlus T.Text) (Registration m) where
    dependencyRequests = rBefore

newtype RequestM a = RequestM { reqUnpack :: StateT Value (ReaderT [T.Text] (WriterT [Registration ResponseM] IO)) a }
    deriving (Monad, Applicative, Functor, MonadIO, MonadState Value)

newtype ResponseM a = ResponseM { resUnpack :: ReaderT Value IO a }
    deriving (Monad, Applicative, Functor, MonadIO, MonadReader Value)

-- Registration

catRegistrations :: (Monad m) => [Registration m] -> Either [Registration m] (m ())
catRegistrations = fmap sequence_ . fmap (map registration) . resolvePlus

-- RequestM

evalRequestM :: RequestM a -> [T.Text] -> IO (Value, [Registration ResponseM])
evalRequestM m args = do
     ((_, req), regs) <- runWriterT . flip runReaderT args . flip runStateT emptyRequest $ reqUnpack m
     return (req, regs)

register :: T.Text -> [T.Text] -> [T.Text] -> ResponseM () -> RequestM ()
register id bef beh reg = register' $ Registration id bef beh reg

register' :: Registration ResponseM -> RequestM ()
register' = RequestM . lift . lift . tell . pure

key' :: AsValue t => T.Text -> Traversal' t (Maybe Value)
key' i = _Object . at i

getReq :: (FromJSON a) => T.Text -> RequestM (Maybe a)
getReq k = undefined <$> use (key' k)

{-
modifyReq :: (FromJSON a, ToJSON a) => T.Text -> (a -> a) -> RequestM ()
modifyReq key f = do
    maybeVal <- getReq key
    case maybeVal of
        Nothing -> return ()
        Just val -> setReq key $ f val
        -}

-- ResponseM

evalResponseM :: ResponseM a -> Value -> IO a
evalResponseM resM resp = flip runReaderT resp $ resUnpack resM

{-
getResp :: (FromJSON a) => T.Text -> ResponseM (Maybe a)
getResp key = ResponseM $ responseLookup key <$> ask
-}
