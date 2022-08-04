{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Queries.Sqlite where

import Data.Fixed
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (Day, addDays, dayOfWeek)
import Data.Time.Clock (UTCTime (..), getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Traversable
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

newtype Weeks = Weeks Int
  deriving (Eq, Ord, Show)

daysSinceSunday :: Day -> Int
daysSinceSunday = (+) 1 . fromEnum . dayOfWeek

utcTimeToDouble :: UTCTime -> Double
utcTimeToDouble t =
  let (MkFixed nWeeksAgoPico) = nominalDiffTimeToSeconds $ utcTimeToPOSIXSeconds t
   in (fromIntegral nWeeksAgoPico :: Double) / (10.0 ^ 12)

previousSunday :: UTCTime -> UTCTime
previousSunday today =
  let diff = case daysSinceSunday (utctDay today) of
        0 -> 7
        n -> n
   in UTCTime (addDays (fromIntegral $ negate diff) (utctDay today)) 0

previousSets :: Weeks -> Connection -> IO [[(Int, Int, Int, Double, Int, Int)]]
previousSets (Weeks w) conn = do
  now <- getCurrentTime
  let days = utcTimeToDouble <$> iterate previousSunday now
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

allWorkouts :: Connection -> IO [(Int, String)]
allWorkouts conn =
  query_
    conn
    [sql|
        select * from workout
    |]
