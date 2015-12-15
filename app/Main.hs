{-# LANGUAGE OverloadedStrings #-}

import Lib

import Control.Error.Util
import qualified Data.ByteString.Lazy.Char8 as BLIO
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Conduit
import System.IO
import Control.Exception
import System.Console.Haskeline
import Data.Maybe
import Data.Aeson.Lens
import Lens.Family2
import Data.Function (fix)

main :: IO ()
main = do 
    (school,naam,ww) <- runInputT defaultSettings $ do
        school <- BLIO.pack . fromMaybe "" <$> getInputLine "School: "
        naam <- BLIO.pack . fromMaybe "" <$> getInputLine "Gebruikersnaam: "
        ww <- BLIO.pack . fromMaybe "" <$> getPassword (Just '*') "Wachtwoord: "
        return (school,naam,ww)
    login naam ww 

login :: BL.ByteString -> BL.ByteString -> IO ()
login naam ww = magisterLogin (toLogin naam ww) >>=
    (\jar -> getHttp (hush jar) (apiUrl ++ "account")) >>=
    either putStrLn (print . fromJust . findId . responseBody) 

findId :: AsValue a => a -> Maybe Integer
findId inputJSON = inputJSON ^? key "Persoon" . key "Id" . _Integer 
