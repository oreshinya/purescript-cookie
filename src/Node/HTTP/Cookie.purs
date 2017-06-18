module Node.HTTP.Cookie where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Array (snoc)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Node.HTTP (HTTP, Response, setHeader)



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
