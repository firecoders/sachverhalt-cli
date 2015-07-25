module Sachverhalt.Client.Plugins
( Plugin(..)
) where

import qualified Data.Text as T
import Sachverhalt.Client.Monads (Registration, RequestM)

type PluginVersion = T.Text
type PluginIdentifier = T.Text

data Plugin = Plugin
    { pRequestReg :: Registration RequestM
    --, pCheckRemotePlugins :: [(PluginVersion, PluginIdentifier)] -> Satisfaction
    }
