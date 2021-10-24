{-# LANGUAGE QuasiQuotes #-}

module Queries.Sqlite where

import Data.Text
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ
import Database.SQLite.Simple.Time
import GHC.Int (Int64)

insertSet :: Connection -> (Int, Int, Double, Int, Int) -> IO ()
insertSet conn (setName, setReps, setDate, setWeight, setIntensity) = do
  execute
    conn
    [sql|
        insert into workout_set (workout, reps, date, weight, intensity)
        values (?,?,?,?, ?);
    |]
    (setName, setReps, setDate, setWeight, setIntensity)

insertMuscle :: Connection -> (String, Int, Int, Int, Int) -> IO ()
insertMuscle conn =
  execute
    conn
    [sql|
        insert into muscle (name, min_rep, max_rep, min_vol, max_vol)
        values (?, ?, ?, ?, ?);
    |]

insertPrimaryMuscle :: Connection -> (Int, Int) -> IO ()
insertPrimaryMuscle conn =
  execute
    conn
    [sql|
        insert into primary_muscle (workout, muscle)
        values (?, ?);
    |]

insertWorkout :: Connection -> Text -> IO ()
insertWorkout conn val =
  execute
    conn
    [sql|
        insert into workout (name)
        values (?);
    |]
    (Only val)

allWorkouts :: Connection -> IO [(Int, String, Int, Int, Int)]
allWorkouts conn =
  query_
    conn
    [sql|
        select primary_muscle.workout_id, muscle.muscle_id, muscle.min_rep, muscle.max_rep
        from primary_muscle
        join muscle on primary_muscle.muscle = muscle.muscle_id
        join workout on primary_muscle.workout = workout.workout_id
    |]

allWorkoutSets :: Connection -> IO [(Int, Int, Int, Double, Int, Int)]
allWorkoutSets conn =
  query_
    conn
    [sql|
        select * from workout_set order by date desc
    |]
