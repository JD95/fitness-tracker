module Workout where

import Data.Argonaut (class DecodeJson, class EncodeJson, jsonEmptyObject, decodeJson, (~>), (:=), (.:))
import Prelude (Unit, Void, pure, unit, bind, const, map, discard, ($), (>>=), (==), (<>))
import Data.Eq
import Data.Ord

newtype WorkoutId = WorkoutId Int
derive newtype instance workoutIdEq :: Eq WorkoutId
derive newtype instance workoutIdOrd :: Ord WorkoutId

newtype Workout = Workout { name :: String }

instance decodeJsonWorkout :: DecodeJson Workout where
  decodeJson json = do
    x <- decodeJson json
    name <- x .: "workoutName"
    pure $ Workout {name} 

instance encodeJsonWorkout :: EncodeJson Workout where
  encodeJson (Workout w) = do
    "workoutName" := w.name
    ~> jsonEmptyObject
