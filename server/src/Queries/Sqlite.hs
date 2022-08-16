{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Queries.Sqlite where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Traversable
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.QQ
import Database.SQLite.Simple.Time
import GHC.Int (Int64)
import Time

insertSet :: Connection -> (Int, Int, Time, Int, Int) -> IO ()
insertSet conn (setWorkout, setReps, setDate, setWeight, setIntensity) = do
  execute
    conn
    [sql|
        insert into workout_set (workout_set_id, workout, reps, date, weight, intensity)
        values (null, ?,?,?,?, ?)
    |]
    (setWorkout, setReps, setDate, setWeight, setIntensity)

allWorkoutSets :: Connection -> IO [(Int, Int, Int, Time, Int, Int)]
allWorkoutSets conn =
  query_
    conn
    [sql|
        select * from workout_set order by date desc
    |]

newtype DbSet = DbSet (Int, Int, Int, Time, Int, Int)
  deriving Show

instance FromRow DbSet where
  fromRow = DbSet <$> fromRow

previousSets :: Weeks -> Connection -> IO [[(Int, Int, Int, Time, Int, Int)]]
previousSets (Weeks w) conn = do
  now <- getCurrentTime
  let days = previousSundays now
  let weekRanges = take (w + 1) $ zip (tail days) days
  for weekRanges $
    query
      conn
      [sql|
          select *
          from workout_set
          where ? < date
            and date < ?
          order by date desc
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

setsForWorkout :: Connection -> Int -> Int -> IO [DbSet]
setsForWorkout conn workout weeksBack = do
  now <- getCurrentTime
  let start = previousSundays now !! weeksBack
  query
    conn
    [sql|
        select *
        from workout_set
        where ? < date
          and workout = ?
          order by date desc

    |]
    (start, workout)

allWorkouts :: Connection -> IO [(Int, String)]
allWorkouts conn =
  query_
    conn
    [sql|
        select * from workout
    |]
