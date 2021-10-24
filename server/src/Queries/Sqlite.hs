{-# LANGUAGE QuasiQuotes #-}

module Queries.Sqlite where

import Data.Text
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ
import Database.SQLite.Simple.Time
import GHC.Int (Int64)

insertSet :: Connection -> (String, Int, Double, Int, Int) -> IO ()
insertSet conn (setName, setReps, setDate, setWeight, setIntensity) = do
  execute
    conn
    [sql|
        insert into workout_set (name, reps, date_of, weight, intensity)
        values (?,?,?,?, ?);
    |]
    (setName, setReps, setDate, setWeight, setIntensity)

insertMuscle :: Connection -> (String, Int, Int, Int, Int) -> IO ()
insertMuscle conn =
  execute
    conn
    [sql|
        insert into muscles (name, min_rep, max_rep, min_vol, max_vol)
        values (?, ?, ?, ?, ?);
    |]

insertPrimaryMuscle :: Connection -> (String, String) -> IO ()
insertPrimaryMuscle conn =
  execute
    conn
    [sql|
        insert into primary_muscles (workout, muscle)
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

allWorkouts :: Connection -> IO [(String, String, Int, Int)]
allWorkouts conn =
  query_
    conn
    [sql|
        select primary_muscles.workout, muscles.name, muscles.min_rep, muscles.max_rep
        from primary_muscles
        join muscles on primary_muscles.muscle = muscles.name
    |]

allWorkoutSets :: Connection -> IO [(String, Int, Double, Int, Int)]
allWorkoutSets conn =
  query_
    conn
    [sql|
        select * from workout_set order by date_of desc
    |]
