{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib
  ( app,
    Env (..),
  )
where

import Control.Monad
import Data.Time.Clock
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Time
import GHC.Word

queryLastWeekVolume :: Connection -> IO [(String, Int)]
queryLastWeekVolume conn =
  query_
    conn
    [sql|
      with previous as (
        select * from workout_set
        where date_trunc('week', date_of + 1)::date - 1 = date_trunc('week', now()::date + 1)::date - 1)
      select pm.muscle, sum(reps)
      from primary_muscles pm, previous p
      where pm.workout = p.name
      group by pm.muscle
      order by pm.muscle
    |]

queryVolumePerExercisePerWeek :: Connection -> IO [(String, Int, Date)]
queryVolumePerExercisePerWeek conn =
  query_
    conn
    [sql|
      with workouts as (
        select distinct name from workout_set
      ) select name, sum(reps), date_trunc('week', date_of + 1)::date - 1
      from workout_set workouts
      where name = workouts.name
      group by name, date_of
      order by date_of desc
    |]

data Env = Env
  { pgHost :: String,
    pgPort :: Word16,
    pgUser :: String,
    pgPass :: String,
    pgDb :: String
  }

app :: Env -> IO ()
app env = do
  conn <-
    connect $
      ConnectInfo
        { connectHost = pgHost env,
          connectPort = pgPort env,
          connectUser = pgUser env,
          connectPassword = pgPass env,
          connectDatabase = pgDb env
        }
  xs <- queryLastWeekVolume conn
  mapM_ print xs
