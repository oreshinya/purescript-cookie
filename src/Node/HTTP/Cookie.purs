module Node.HTTP.Cookie
  ( Payload
  , setCookie
  , getCookie
  , getCookies
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Array (snoc, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split, trim, joinWith)
import Data.StrMap (StrMap, lookup, empty, fromFoldable)
import Data.Tuple (Tuple(..))
import Node.HTTP (HTTP, Request, Response, setHeader, requestHeaders)



type Payload =
  { key :: String
  , value :: String
  , maxAge :: Maybe Int
  , secure :: Boolean
  , httpOnly :: Boolean
  }



setCookie :: forall e. Response -> Payload -> Eff (http :: HTTP | e) Unit
setCookie res pld = setHeader res "Set-Cookie" $ toField pld



toField :: Payload -> String
toField pld = joinWith "; " <<< setHttpOnly pld <<< setSecure pld <<< setMaxAge pld $ [ pld.key <> "=" <> pld.value ]



setMaxAge :: Payload -> Array String -> Array String
setMaxAge pld xs =
  case pld.maxAge of
    Nothing -> xs
    Just i -> snoc xs $ "Max-Age=" <> show i



setSecure :: Payload -> Array String -> Array String
setSecure pld xs =
  if pld.secure
    then snoc xs "Secure"
    else xs



setHttpOnly :: Payload -> Array String -> Array String
setHttpOnly pld xs =
  if pld.httpOnly
    then snoc xs "HttpOnly"
    else xs



getCookie :: Request -> String -> Maybe String
getCookie req key = lookup key $ getCookies req



getCookies :: Request -> StrMap String
getCookies req =
  case (getCookieStr req) of
    Nothing -> empty
    Just str ->
      fromFoldable $ map (trim >>> toTuple) $ split (Pattern ";") str



getCookieStr :: Request -> Maybe String
getCookieStr req = lookup "cookie" $ requestHeaders req



toTuple :: String -> Tuple String String
toTuple str =
  let ary = split (Pattern "=") str
      key = fromMaybe "" $ ary !! 0
      val = fromMaybe "" $ ary !! 1
   in Tuple key val
