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
import qualified Database.SQLite.Simple as SQ
import DbId
import GHC.Word
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Queries.Sqlite as S
import Servant
import Workout
import WorkoutSet

type API =
  ("workouts" :> Get '[JSON] [Id Workout])
    :<|> ("sets" :> Get '[JSON] [Id WorkoutSet])
    :<|> ("sets" :> ReqBody '[JSON] WorkoutSet :> Post '[JSON] ())
    :<|> Raw

data Env = Env {db :: SQ.Connection}

server :: Env -> Server API
server (Env db) = workouts :<|> getSets :<|> postSets :<|> static
  where
    static = serveDirectoryWebApp "../frontend"

    getSets = do
      sets <- liftIO $ S.allWorkoutSets db
      pure [Id i (MkWorkoutSet a b c d e) | (i, a, b, c, d, e) <- sets]

    postSets (MkWorkoutSet workout reps date weight intensity) = do
      liftIO $ void $ S.insertSet db (workout, reps, date, weight, intensity)

    workouts = liftIO $ do
      ws <- S.allWorkouts db
      pure [(Id i (Workout n t mi ma)) | (i, n, t, mi, ma) <- ws]

data Config = Config

app :: Config -> IO ()
app config = do
  conn <- SQ.open "test.db"
  run 8081 (serve (Proxy @API) (server $ Env conn))
