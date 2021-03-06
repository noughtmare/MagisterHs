{-# LANGUAGE OverloadedStrings #-}
module Lib {-(magisterLogin, toLogin)-} where

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Attoparsec.ByteString as AB
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BLIO
import Data.Maybe
import Network.HTTP.Types.Method
import Data.CaseInsensitive
import Control.Monad
import Control.Applicative
import Network.HTTP.Types.Header
import Control.Error.Util

type MyError = String

prettifyJSON :: BL.ByteString -> BL.ByteString
prettifyJSON = encodePretty . fromMaybe "" . AB.maybeResult . AB.parse json . BL.toStrict

school :: String
school = "ichthuslyceum"

apiUrl :: String
apiUrl = "https://" ++ school ++ ".magister.net/api/"

toLogin :: BL.ByteString -> BL.ByteString -> RequestBody
toLogin naam ww = RequestBodyLBS $ "{ Gebruikersnaam:\"" `BL.append` naam `BL.append` 
    "\", Wachtwoord:\"" `BL.append` ww `BL.append` "\", IngelogdBlijven:\"true\"}"

postHttp :: RequestBody -> Maybe CookieJar -> String -> IO (Either MyError (Response BL.ByteString))
postHttp body = myHttp methodPost (Just [(mk "Content-Type", "application/json;charset=UTF-8")]) (Just body)

getHttp :: Maybe CookieJar -> String -> IO (Either MyError (Response BL.ByteString))
getHttp = myHttp methodGet Nothing Nothing

magisterLogin :: RequestBody -> IO (Either MyError CookieJar)
magisterLogin login = liftM (fmap responseCookieJar) 
    (getCookieJar (apiUrl ++ "sessies/huidige") >>= (\jar -> postHttp login (hush jar) (apiUrl ++ "sessies")))

getCookieJar :: String -> IO (Either MyError CookieJar)
getCookieJar = (fmap . fmap) responseCookieJar . getHttp (Just $ createCookieJar mempty)

myHttp :: Method 
       -> Maybe RequestHeaders
       -> Maybe RequestBody
       -> Maybe CookieJar 
       -> String  
       -> IO (Either MyError (Response BL.ByteString))
myHttp method headers body jar url = case parseUrl url of
        Left err -> return . Left . show $ err
        Right req -> do
            man <- newManager tlsManagerSettings
            let req' = req { method = method
                           , requestHeaders = fromMaybe mempty headers
                           , requestBody = fromMaybe mempty body
                           , cookieJar = jar }
            Right <$> httpLbs req' man
