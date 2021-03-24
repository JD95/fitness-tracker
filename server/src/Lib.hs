{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( app,
    Config (..),
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Maybe
import Data.Proxy
import Data.Time.Clock
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Time
import GHC.Word
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Queries as Q
import Servant
import WorkoutSet

type API = "sets" :> Get '[JSON] [WorkoutSet]

data Env = Env {db :: Connection}

server :: Env -> Server API
server (Env db) = do
  sets <- liftIO $ Q.allWorkoutSets db
  pure . catMaybes $ validate <$> sets
  where
    validate (_, _, NegInfinity, _) = Nothing
    validate (_, _, PosInfinity, _) = Nothing
    validate (a, b, Finite c, d) = Just $ MkWorkoutSet a b (UTCTime c 0) d

data Config = Config
  { pgHost :: String,
    pgPort :: Word16,
    pgUser :: String,
    pgPass :: String,
    pgDb :: String
  }

app :: Config -> IO ()
app config = do
  conn <-
    connect $
      ConnectInfo
        { connectHost = pgHost config,
          connectPort = pgPort config,
          connectUser = pgUser config,
          connectPassword = pgPass config,
          connectDatabase = pgDb config
        }
  run 8081 (serve (Proxy @API) (server $ Env conn))
