{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Network.HTTP.Conduit
import Data.ByteString.Lazy as BL
import Data.ByteString as B
import Data.Text.Encoding 
import Data.Text as T
import Data.Attoparsec.ByteString
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8 as BLIO
import Data.Maybe
import Network.HTTP.Types.Method
import GHC.Generics

data Login = Login {
           gebruikersnaam :: String,
           wachtwoord :: String,
           ingelogdBlijven :: Bool
} deriving (Show)

instance ToJSON Login where
        toJSON (Login gebruikersnaam wachtwoord ingelogdBlijven) =
            object ["Gebruikersnaam" .= gebruikersnaam
                   ,"Wachtwoord" .= wachtwoord
                   ,"IngelogdBlijven" .= ingelogdBlijven]

someFunc :: IO ()
someFunc = Prelude.putStrLn "someFunc"

putHttpJson :: String -> IO ()
putHttpJson url = BLIO.putStrLn . encodePretty . fromJust . maybeResult . parse json . BL.toStrict =<< simpleHttp url

testFunc :: String -> String -> BL.ByteString
testFunc gebruikersnaam wachtwoord = encode (Login {gebruikersnaam=gebruikersnaam, wachtwoord=wachtwoord, ingelogdBlijven=True})
