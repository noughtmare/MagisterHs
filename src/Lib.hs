{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Text.Encoding 
import qualified Data.Text as T
import qualified Data.Attoparsec.ByteString as AB
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BLIO
import Data.Maybe
import Network.HTTP.Types.Method
import GHC.Generics
import Control.Monad.IO.Class
import Data.CaseInsensitive

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

putHttpJson :: String -> IO ()
putHttpJson url = BLIO.putStrLn . prettifyJSON =<< simpleHttp url

prettifyJSON :: BL.ByteString -> BL.ByteString
prettifyJSON = encodePretty . fromResult . AB.parse json . BL.toStrict

fromResult :: AB.Result a -> a
fromResult = fromJust . AB.maybeResult

toLogin :: String -> String -> RequestBody
toLogin gebruikersnaam wachtwoord = RequestBodyLBS $ encode (Login { gebruikersnaam=gebruikersnaam
                                                  , wachtwoord=wachtwoord
                                                  , ingelogdBlijven=True})

postHttp :: String -> RequestBody -> Maybe CookieJar -> IO (Maybe (Response BL.ByteString))
postHttp url body cookies = do
        case parseUrl url of
            Nothing -> do
                putStrLn "Invalid Url"
                return Nothing
            Just req -> withManager $ \manager -> do
                let reqPost = req { method = methodPost
                                  , requestBody = body
                                  , requestHeaders = [(mk "Content-Type", "application/json;charset=UTF-8")]
                                  , cookieJar = cookies}
                res <- httpLbs reqPost manager
                return (Just res)

magisterLogin :: RequestBody -> IO (Maybe CookieJar)
magisterLogin login = return . fmap responseCookieJar 
                          =<< postHttp (apiUrl ++ "sessies/") login 
                          =<< getCookieJar (apiUrl ++ "sessies/huidige/")

{-
deleteHttp :: String -> Maybe CookieJar -> IO (Maybe (Response BL.ByteString))
deleteHttp url cookieJar = do
        case parseUrl url of
            Nothing -> do 
                putStrLn "Invalid url"
                return Nothing
            Just req -> withManager $ \manager -> do
                let reqDelete = req { method = methodDelete
                                    , cookieJar = cookieJar }
                res <- httpLbs reqDelete manager
                return (Just res)
-}
getHttp :: String -> Maybe CookieJar -> IO (Maybe (Response BL.ByteString))
getHttp url cookieJar = do
        case parseUrl url of
            Nothing -> do 
                putStrLn "Invalid url"
                return Nothing
            Just req -> withManager $ \manager -> do
                let reqGet = req { method = methodGet
                                 , cookieJar = cookieJar }
                res <- httpLbs reqGet manager
                return (Just res)

getCookieJar :: String -> IO (Maybe CookieJar)
getCookieJar url = do
        case parseUrl url of
            Nothing -> do 
                putStrLn "Invalid url"
                return Nothing
            Just req -> withManager $ \manager -> do
                let reqCookieJar = req { method = methodGet }
                res <- http reqCookieJar manager
                return (Just (responseCookieJar res))
