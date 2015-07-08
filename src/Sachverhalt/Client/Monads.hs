{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Sachverhalt.Client.Monads
    ( Registration(..)
    , catRegistrations
    , register
    , RequestM
    , evalRequestM
    , setReq
    , getReq
    , ResponseM
    , evalResponseM
    , getResp
    ) where

import Algorithms.Dependant (Dependant(..), DependantPlus(..), resolvePlus)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.State (StateT, runStateT, modify, gets)
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Applicative (pure)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import Sachverhalt.Client.Types (Request, emptyRequest, requestInsert,
  requestLookup, Response, responseLookup)

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

newtype RequestM a = RequestM { reqUnpack :: StateT Request (ReaderT [T.Text] (WriterT [Registration ResponseM] IO)) a }
    deriving (Monad, Applicative, Functor, MonadIO)

newtype ResponseM a = ResponseM { resUnpack :: ReaderT Response IO a }
    deriving (Monad, Applicative, Functor, MonadIO)

-- Registration

catRegistrations :: (Monad m) => [Registration m] -> Either [Registration m] (m ())
catRegistrations = fmap sequence_ . fmap (map registration) . resolvePlus

-- RequestM

evalRequestM :: RequestM a -> [T.Text] -> IO (Request, [Registration ResponseM])
evalRequestM m args = do
     ((_, req), regs) <- runWriterT . flip runReaderT args . flip runStateT emptyRequest $ reqUnpack m
     return (req, regs)

register :: T.Text -> [T.Text] -> [T.Text] -> ResponseM () -> RequestM ()
register id bef beh reg = register' $ Registration id bef beh reg

register' :: Registration ResponseM -> RequestM ()
register' = RequestM . lift . lift . tell . pure

setReq :: (ToJSON a) => T.Text -> a -> RequestM ()
setReq key val = RequestM . modify $ requestInsert key val

getReq :: (FromJSON a) => T.Text -> RequestM (Maybe a)
getReq key = RequestM . gets $ requestLookup key

modifyReq :: (FromJSON a, ToJSON a) => T.Text -> (a -> a) -> RequestM ()
modifyReq key f = do
    maybeVal <- getReq key
    case maybeVal of
        Nothing -> return ()
        Just val -> setReq key $ f val

-- ResponseM

evalResponseM :: ResponseM a -> Response -> IO a
evalResponseM resM resp = flip runReaderT resp $ resUnpack resM

getResp :: (FromJSON a) => T.Text -> ResponseM (Maybe a)
getResp key = ResponseM $ responseLookup key <$> ask
