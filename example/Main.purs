module Main where


import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Conveyor (run)
import Conveyor.Argument (RawData(..))
import Conveyor.Respondable (class Respondable, Responder(..))
import Conveyor.Servable (class Servable, serve)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Node.HTTP (HTTP, ListenOptions, requestHeaders)
import Node.HTTP.Cookie (setCookie, getCookie, getCookies)
import Node.Process (PROCESS, lookupEnv)
import Simple.JSON (class WriteForeign, write)



data Result r
  = Success { status :: Int, body :: r }
  | Failure { status :: Int, message :: String }

type MyJson = { content :: String }



instance respondableResult :: WriteForeign r => Respondable (Result r) where
  toResponder (Success s) =
    Responder
      { contentType: "application/json"
      , code: s.status
      , body: write s.body
      }
  toResponder (Failure f) =
    Responder
      { contentType: "application/json"
      , code: f.status
      , body: write { messages: [ f.message ] }
      }
  fromError _ = Failure { status: 500, message: "Internal server error ;)" }



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



myJson :: forall e. Aff e (Result MyJson)
myJson = pure $ Success
  { status: 200
  , body: { content: "test content :)" }
  }



newtype Cookie s = Cookie s

instance servableCookie :: Servable c (http :: HTTP, console :: CONSOLE | e) s => Servable c (http :: HTTP, console :: CONSOLE | e) (Cookie s) where
  serve (Cookie servable) ctx rawData@(RawData rd) = do
    liftEff do
      log $ show $ requestHeaders rd.req
      log $ show $ getCookies rd.req
      log $ show $ getCookie rd.req "id"
      setCookie rd.res
        { key: "id"
        , value: "fjdkaflk"
        , maxAge: Just 60
        , secure: false
        , httpOnly: true
        }
    serve servable ctx rawData



main :: forall e. Eff (process :: PROCESS, console :: CONSOLE, http :: HTTP | e ) Unit
main = do
  config <- getConfig
  run config (Cookie { myJson })
