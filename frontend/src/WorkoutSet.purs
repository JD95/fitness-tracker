module WorkoutSet where

import Prelude
import Data.Eq
import Data.Ord
import Data.Argonaut (class DecodeJson, class EncodeJson, jsonEmptyObject, decodeJson, (~>), (:=), (.:))
import Data.Newtype

newtype WorkoutSetId = WorkoutSetId Int

derive newtype instance workoutSetIdEq :: Eq WorkoutSetId
derive newtype instance workoutSetIdOrd :: Ord WorkoutSetId

newtype WorkoutSet = WorkoutSet
  { workout :: Int
  , reps :: Int
  , date :: Number
  , weight :: Int
  , intensity :: Int
  }

derive instance newtypeWorkoutSet :: Newtype WorkoutSet _

instance decodeJsonWorkoutSet :: DecodeJson WorkoutSet where
  decodeJson json = do
    x <- decodeJson json
    workout <- x .: "setWorkout"
    reps <- x .: "setReps"
    date <- x .: "setDate"
    weight <- x .: "setWeight"
    intensity <- x .: "setIntensity"
    pure $ WorkoutSet { workout, reps, date, weight, intensity }

instance encodeJsonWorkoutSet :: EncodeJson WorkoutSet where
  encodeJson (WorkoutSet set) = do
    "setWorkout" := set.workout
    ~> "setReps" := set.reps
    ~> "setDate" := set.date
    ~> "setWeight" := set.weight
    ~> "setIntensity" := set.intensity
    ~> jsonEmptyObject
