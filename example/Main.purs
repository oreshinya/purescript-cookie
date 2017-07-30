module Main where


import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Conveyor (run)
import Conveyor.Handler (Handler)
import Conveyor.Responsable (Result, result, respond, errorMsg)
import Conveyor.Servable (class Servable, serve)
import Data.Foreign.Class (class Encode)
import Data.Foreign.Generic (defaultOptions, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Node.HTTP (HTTP, ListenOptions, requestHeaders)
import Node.HTTP.Cookie (setCookie, getCookie, getCookies)
import Node.Process (PROCESS, lookupEnv)



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
myJson = pure $ result 200 $ MyJson { content: "test content :)" }



newtype Cookie s = Cookie s

instance servableCookie :: Servable (console :: CONSOLE | e) s => Servable (console :: CONSOLE | e) (Cookie s) where
  serve (Cookie handler) req res path = Just do
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
    case serve handler req res path of
      Nothing -> respond res $ errorMsg 500 "Something went wrong."
      Just s -> s



main :: forall e. Eff (process :: PROCESS, console :: CONSOLE, exception :: EXCEPTION, ref :: REF, http :: HTTP | e ) Unit
main = do
  config <- getConfig
  run config $ Cookie { myJson }
