module PrimaryMuscle where

import Data.Argonaut (class DecodeJson, class EncodeJson, jsonEmptyObject, decodeJson, (~>), (:=), (.:))
import Prelude (Unit, Void, pure, unit, bind, const, map, discard, ($), (>>=), (==), (<>))

newtype PrimaryMuscle = PrimaryMuscle 
  { workout :: Int
  , muscle :: Int
  }

instance decodeJsonMuscle :: DecodeJson PrimaryMuscle where
  decodeJson json = do
    x <- decodeJson json
    workout <- x .: "primaryMuscleWorkout"
    muscle <- x .: "primaryMuscleMuscle"
    pure $ PrimaryMuscle {workout, muscle}

instance encodeJsonMuscle :: EncodeJson PrimaryMuscle where
  encodeJson (PrimaryMuscle w) = do
    "primaryMuscleWorkout" := w.workout
    ~> "priamryMuscleMuscle" := w.muscle
    ~> jsonEmptyObject
