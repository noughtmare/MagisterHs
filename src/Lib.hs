{-# LANGUAGE OverloadedStrings #-}
module Lib (magisterLogin, someFunc) where

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
--import Data.Text.Encoding 
import qualified Data.Text as T
import qualified Data.Attoparsec.ByteString as AB
import Data.Aeson
--import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BLIO
import Data.Maybe
import Network.HTTP.Types.Method
--import Control.Monad.IO.Class
import Data.CaseInsensitive
import Control.Monad
import Control.Applicative
import Network.HTTP.Types.Header
import Control.Error.Util

type MyError = String

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

school :: String
school = "ichthuslyceum"

apiUrl :: String
apiUrl = "https://" ++ school ++ ".magister.net/api/"

someFunc :: IO ()
someFunc = Prelude.putStrLn "someFunc"

toLogin :: String -> String -> RequestBody
toLogin gebruikersnaam wachtwoord = RequestBodyLBS $ encode Login 
    { gebruikersnaam=gebruikersnaam, wachtwoord=wachtwoord, ingelogdBlijven=True}

postHttp :: RequestBody -> Maybe CookieJar -> String -> IO (Either MyError (Response BL.ByteString))
postHttp body = myHttp methodPost (Just [(mk "Content-Type", "application/json;charset=UTF-8")]) (Just body)

magisterLogin :: RequestBody -> IO (Either MyError CookieJar)
magisterLogin login = liftM (fmap responseCookieJar) 
    (getCookieJar (apiUrl ++ "sessies/huidige") >>= (\jar -> postHttp login (hush jar) (apiUrl ++ "sessies")))

getCookieJar :: String -> IO (Either MyError CookieJar)
getCookieJar url = fmap responseCookieJar <$> myHttp methodGet mempty mempty (Just $ createCookieJar mempty) url

myHttp :: Method 
       -> Maybe RequestHeaders
       -> Maybe RequestBody
       -> Maybe CookieJar 
       -> String  
       -> IO (Either MyError (Response BL.ByteString))
myHttp method headers body jar url = case parseUrl url of
        Nothing -> return . Left $ "Invalid url"
        Just req -> do
            man <- newManager tlsManagerSettings
            let req' = req { method = method
                           , requestHeaders = fromMaybe mempty headers
                           , requestBody = fromMaybe mempty body
                           , cookieJar = jar }
            liftM Right $ httpLbs req' man

