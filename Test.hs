{-# LANGUAGE OverloadedStrings #-}

module Test where

import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Maybe (fromJust)
import Network
import Sachverhalt.Client.DefaultMain
import Sachverhalt.Client.Monads
import Sachverhalt.Client.Plugins
import System.IO

t :: T.Text -> T.Text
t = id

main = defaultMain config
config = Configuration plugins $ connectTo "localhost" (PortNumber 2000)
plugins =
    [ Plugin
        { pRequestReg = Registration
            { rIdentifier = "bestplugin"
            , rBefore = []
            , rBehind = []
            , registration = do
                setReq "key" $ t "val"
                liftIO . putStrLn . fromJust =<< getReq "key"
                register "bestplugin" [] [] $ do
                    liftIO $ putStrLn "Hello World!"
                    maybeVal <- getResp "resKey"
                    case maybeVal of
                        Nothing -> liftIO $ putStrLn "no val"
                        Just x -> liftIO $ putStrLn x
            }
        }
    ]
