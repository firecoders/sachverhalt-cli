module Sachverhalt.Client.DefaultMain
( defaultMain
, Configuration(..)
) where

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Applicative ((<$>))
import Control.Monad (forM_, void)
import Sachverhalt.Client.Monads
import Sachverhalt.Client.Plugins
import Sachverhalt.Client.Types (Request, Response, dumpRequest, parseResponse)
import System.Environment (getArgs)
import System.IO (Handle)

data Configuration = Configuration
    { cPlugins :: [Plugin]
    , cConnect :: IO Handle
    }

defaultMain :: Configuration -> IO ()
defaultMain conf = case catRegistrations . map pRequestReg . cPlugins $ conf of
    Left invalids -> putStr $ buildInvalidRegistrationsErrmsg "Request" invalids
    Right reqM -> runRequestM conf reqM

runRequestM :: Configuration -> RequestM () -> IO ()
runRequestM conf reqM = do
    (request, registrations) <- getArgsTIO >>= evalRequestM reqM
    case catRegistrations registrations of
        Left invalids -> putStr $ buildInvalidRegistrationsErrmsg "Response" invalids
        Right resM -> do
            maybeResp <- sendRequest conf request
            case maybeResp of
                Nothing -> putStrLn "Error: Could not parse server response"
                Just resp -> evalResponseM resM resp

buildInvalidRegistrationsErrmsg :: String -> [Registration m] -> String
buildInvalidRegistrationsErrmsg desc regs = unlines $
    ("Error: Some dependencies of the following " ++ desc
    ++ "-Registrations could not be resolved:") :
    map show regs


sendRequest :: Configuration -> Request -> IO (Maybe Response)
sendRequest conf req = do
    h <- cConnect conf
    parseResponse <$> handleInteract h (dumpRequest req)

handleInteract :: Handle -> B.ByteString -> IO B.ByteString
handleInteract h str = B.hPutStrLn h str >> B.hGetContents h

getArgsTIO :: IO [T.Text]
getArgsTIO = map T.pack <$> getArgs
