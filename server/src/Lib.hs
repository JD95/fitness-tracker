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
import Workout
import WorkoutSet

type API =
  ("workouts" :> Get '[JSON] [Workout])
    :<|> ("sets" :> Get '[JSON] [WorkoutSet])
    :<|> ("sets" :> ReqBody '[JSON] WorkoutSet :> Post '[JSON] ())
    :<|> Raw

data Env = Env {db :: Connection}

server :: Env -> Server API
server (Env db) = workouts :<|> getSets :<|> postSets :<|> static
  where
    static = serveDirectoryWebApp "../frontend"

    getSets = do
      sets <- liftIO $ Q.allWorkoutSets db
      pure [MkWorkoutSet a b c d e | (a, b, c, d, e) <- sets]

    postSets (MkWorkoutSet workout reps date weight intensity) = do
      liftIO $ void $ Q.insertSet db workout reps date weight intensity

    workouts = liftIO $ do
      ws <- Q.allWorkouts db
      pure [Workout n t mi ma | (n, t, mi, ma) <- ws]

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
