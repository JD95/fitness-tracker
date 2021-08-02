{-# LANGUAGE QuasiQuotes #-}

module Queries where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Time
import GHC.Int (Int64)

insertSet :: Connection -> String -> Int -> Date -> Int -> IO Int64
insertSet conn setName setReps setDate setWeight = do
  execute
    conn
    [sql|
        insert into workout_set (name, reps, date_of, weight)
        values (?,?,?,?);
    |]
    (setName, setReps, setDate, setWeight)

allWorkouts :: Connection -> IO [(String, String, Int, Int)]
allWorkouts conn =
  query_
    conn
    [sql|
        select primary_muscles.workout, muscles.name, muscles.min_rep, muscles.max_rep
        from primary_muscles 
        join muscles on primary_muscles.muscle = muscles.name
    |]

allWorkoutSets :: Connection -> IO [(String, Int, Date, Int)]
allWorkoutSets conn =
  query_
    conn
    [sql|
        select * from workout_set order by date_of desc
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
