module Main where


import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Conveyor (run)
import Conveyor.Handler (Handler)
import Conveyor.Respondable (class Respondable, ConveyorError(..), respond)
import Conveyor.Servable (class Servable, serve)
import Data.Foreign.Class (class Encode)
import Data.Foreign.Generic (defaultOptions, encodeJSON, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Node.HTTP (HTTP, ListenOptions, requestHeaders)
import Node.HTTP.Cookie (setCookie, getCookie, getCookies)
import Node.Process (PROCESS, lookupEnv)



data Result r
  = Success { status :: Int, body :: r }
  | Failure { status :: Int, message :: String }

instance respondableResult :: Encode r => Respondable (Result r) where
  statusCode (Success s) = s.status
  statusCode (Failure f) = f.status

  encodeBody (Success s) = encodeJSON s.body
  encodeBody (Failure f) = "{ \"message\": [\"" <> f.message <> "\"] }"

  systemError _ = Failure { status: 500, message: "Internal server error" }

newtype MyJson = MyJson { content :: String }

derive instance genericMyJson :: Generic MyJson _

instance encodeMyJson :: Encode MyJson where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }



getHostname :: forall e. Eff e String
getHostname = pure "0.0.0.0"



getPort :: forall e. Eff (process :: PROCESS | e) Int
getPort = do
  portStr <- lookupEnv "PORT"
  pure $ case (map fromString portStr) of
    Just (Just i) -> i
    _ -> 3000



getBacklog :: forall e. Eff e (Maybe Int)
getBacklog = pure Nothing



getConfig :: forall e. Eff (process :: PROCESS | e) ListenOptions
getConfig = do
  hostname <- getHostname
  port <- getPort
  backlog <- getBacklog
  pure { hostname, port, backlog }



myJson :: forall e. Handler e (Result MyJson)
myJson = pure $ Success
  { status: 200
  , body: MyJson { content: "test content :)" }
  }



newtype Cookie s = Cookie s

instance servableCookie :: Servable c (console :: CONSOLE | e) s => Servable c (console :: CONSOLE | e) (Cookie s) where
  serve ctx (Cookie handler) req res path = Just do
    log $ show $ requestHeaders req
    log $ show $ getCookies req
    log $ show $ getCookie req "id"
    setCookie res
      { key: "id"
      , value: "fjdkaflk"
      , maxAge: Just 60
      , secure: false
      , httpOnly: true
      }
    case serve ctx handler req res path of
      Nothing -> respond res $ ConveyorError 500 "Something went wrong."
      Just s -> s



main :: forall e. Eff (process :: PROCESS, console :: CONSOLE, exception :: EXCEPTION, ref :: REF, http :: HTTP | e ) Unit
main = do
  config <- getConfig
  run (Cookie { myJson }) config
