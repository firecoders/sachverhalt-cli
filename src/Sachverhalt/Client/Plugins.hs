module Sachverhalt.Client.Plugins
( Plugin(..)
) where

import Sachverhalt.Client.Monads (Registration, RequestM)

data Plugin = Plugin
    { pRequestReg :: Registration RequestM
    }
