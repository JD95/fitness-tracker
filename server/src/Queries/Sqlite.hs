{-# LANGUAGE QuasiQuotes #-}

module Queries.Sqlite where

import Data.Text
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ
import Database.SQLite.Simple.Time
import GHC.Int (Int64)

insertSet :: Connection -> (Int, Int, Double, Int, Int) -> IO ()
insertSet conn (setWorkout, setReps, setDate, setWeight, setIntensity) = do
  execute
    conn
    [sql|
        insert into workout_set (workout_set_id, workout, reps, date, weight, intensity)
        values (null, ?,?,?,?, ?)
    |]
    (setWorkout, setReps, setDate, setWeight, setIntensity)

allWorkoutSets :: Connection -> IO [(Int, Int, Int, Double, Int, Int)]
allWorkoutSets conn =
  query_
    conn
    [sql|
        select * from workout_set order by date desc
    |]

insertMuscle :: Connection -> (String, Int, Int, Int, Int) -> IO ()
insertMuscle conn =
  execute
    conn
    [sql|
        insert into muscle (muscle_id, name, min_rep, max_rep, min_vol, max_vol)
        values (null, ?, ?, ?, ?, ?);
    |]

allMuscles :: Connection -> IO [(Int, String, Int, Int, Int, Int)]
allMuscles conn =
  query_
    conn
    [sql|
        select * from muscle
    |]

insertPrimaryMuscle :: Connection -> (Int, Int) -> IO ()
insertPrimaryMuscle conn =
  execute
    conn
    [sql|
        insert into primary_muscle (workout, muscle)
        values (?, ?);
    |]

allPrimaryMuscles :: Connection -> IO [(Int, Int)]
allPrimaryMuscles conn =
  query_
    conn
    [sql|
        select * from primary_muscle
    |]

insertWorkout :: Connection -> Text -> IO ()
insertWorkout conn val =
  execute
    conn
    [sql|
        insert into workout (workout_id, name)
        values (null, ?);
    |]
    (Only val)

allWorkouts :: Connection -> IO [(Int, String)]
allWorkouts conn =
  query_
    conn
    [sql|
        select * from workout
    |]
