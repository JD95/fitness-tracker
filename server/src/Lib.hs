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

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(Proxy))
import qualified Database.SQLite.Simple as SQ
import DbId (Id(Id))
import Muscle (Muscle(Muscle))
import Network.Wai.Handler.Warp (run)
import PrimaryMuscle (PrimaryMuscle(PrimaryMuscle))
import qualified Queries.Sqlite as S
import Servant
import Workout (Workout(Workout))
import WorkoutSet (WorkoutSet(MkWorkoutSet))

type API =
  ("workouts" :> Get '[JSON] [Id Workout])
    :<|> ("muscles" :> Get '[JSON] [Id Muscle])
    :<|> ("primary-muscles" :> Get '[JSON] [PrimaryMuscle])
    :<|> ("sets" :> QueryParam "weeks" Int :> Get '[JSON] [[Id WorkoutSet]])
    :<|> ("sets" :> ReqBody '[JSON] WorkoutSet :> Post '[JSON] (Id WorkoutSet))
    :<|> Raw

newtype Env = Env {db :: SQ.Connection}

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

    getSets mweeks = do
      let weeks = fromMaybe 0 mweeks
      setsPerWeek <- liftIO $ S.previousSets (S.Weeks weeks) db
      pure $ fmap (\set -> [Id i (MkWorkoutSet a b c d e) | (i, a, b, c, d, e) <- set]) setsPerWeek

    postSets ws@(MkWorkoutSet workout reps date weight intensity) = do
      liftIO $ do
        S.insertSet db (workout, reps, date, weight, intensity)
        i <- SQ.lastInsertRowId db
        pure $ Id (fromIntegral i) ws

    workouts = liftIO $ do
      ws <- S.allWorkouts db
      pure [Id i (Workout n) | (i, n) <- ws]

data Config = Config

app :: Config -> IO ()
app config = do
  SQ.withConnection "test.db" $ \conn -> do
    SQ.setTrace conn $ Just print
    run 8081 (serve (Proxy @API) (server $ Env conn))
