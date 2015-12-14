import Lib

import Control.Error.Util
import qualified Data.ByteString.Lazy.Char8 as BLIO
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Conduit
import System.IO
import Control.Exception
import System.Console.Haskeline
import Data.Maybe

main :: IO ()
main = do 
    (school,naam,ww) <- runInputT defaultSettings $ do
        school <- BLIO.pack . fromMaybe "" <$> getInputLine "School: "
        naam <- BLIO.pack . fromMaybe "" <$> getInputLine "School: "
        ww <- BLIO.pack . fromMaybe "" <$> getPassword (Just '*') "Wachtwoord: "
        return (school,naam,ww)
    login naam ww
    
login :: BL.ByteString -> BL.ByteString -> IO ()
login naam ww =
    either putStrLn (BLIO.putStrLn . prettifyJSON . responseBody) 
        =<< (\jar -> getHttp (hush jar) (apiUrl ++ "account"))
        =<< magisterLogin (toLogin naam ww)

