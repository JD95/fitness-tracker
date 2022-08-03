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
import Muscle
import Network.Wai
import Network.Wai.Handler.Warp
import PrimaryMuscle
import qualified Queries.Sqlite as S
import Servant
import Workout
import WorkoutSet

type API =
  ("workouts" :> Get '[JSON] [Id Workout])
    :<|> ("muscles" :> Get '[JSON] [Id Muscle])
    :<|> ("primary-muscles" :> Get '[JSON] [PrimaryMuscle])
    :<|> ("sets" :> Get '[JSON] [Id WorkoutSet])
    :<|> ("sets" :> ReqBody '[JSON] WorkoutSet :> Post '[JSON] (Id WorkoutSet))
    :<|> Raw

data Env = Env {db :: SQ.Connection}

server :: Env -> Server API
server (Env db) = workouts :<|> getMuscles :<|> getPrimaryMuscles :<|> getSets :<|> postSets :<|> static
  where
    static = serveDirectoryWebApp "../frontend"

    getMuscles = do
      muscles <- liftIO $ S.allMuscles db
      pure [Id i (Muscle a b c d e) | (i, a, b, c, d, e) <- muscles]

    getPrimaryMuscles = do
      primaryMuscles <- liftIO $ S.allPrimaryMuscles db
      pure [PrimaryMuscle a b | (a, b) <- primaryMuscles]

    getSets = do
      sets <- liftIO $ S.allWorkoutSets db
      -- sets <- liftIO $ S.previousSets (S.Weeks 1) db
      pure [Id i (MkWorkoutSet a b c d e) | (i, a, b, c, d, e) <- sets]

    postSets ws@(MkWorkoutSet workout reps date weight intensity) = do
      liftIO $ do
        S.insertSet db (workout, reps, date, weight, intensity)
        i <- SQ.lastInsertRowId db
        pure $ Id (fromIntegral i) ws

    workouts = liftIO $ do
      ws <- S.allWorkouts db
      pure [(Id i (Workout n)) | (i, n) <- ws]

data Config = Config

app :: Config -> IO ()
app config = do
  SQ.withConnection "test.db" $ \conn -> do
    SQ.setTrace conn $ Just print
    run 8081 (serve (Proxy @API) (server $ Env conn))
