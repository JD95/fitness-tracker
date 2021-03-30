{-# LANGUAGE QuasiQuotes #-}

module Queries where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Time

allWorkouts :: Connection -> IO [Only String]
allWorkouts conn =
  query_
    conn
    [sql|
        select * from workout
    |]

allWorkoutSets :: Connection -> IO [(String, Int, Date, Int)]
allWorkoutSets conn =
  query_
    conn
    [sql|
        select * from workout_set
    |]

lastWeekVolume :: Connection -> IO [(String, Int)]
lastWeekVolume conn =
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

volumePerExercisePerWeek :: Connection -> IO [(String, Int, Date)]
volumePerExercisePerWeek conn =
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
