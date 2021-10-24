module Muscle where

import Data.Argonaut (class DecodeJson, class EncodeJson, jsonEmptyObject, decodeJson, (~>), (:=), (.:))
import Prelude (Unit, Void, pure, unit, bind, const, map, discard, ($), (>>=), (==), (<>))
import Data.Eq
import Data.Ord

newtype MuscleId
  = MuscleId Int

derive newtype instance muscleIdEq :: Eq MuscleId
derive newtype instance muscleIdOrd :: Ord MuscleId

newtype Muscle = Muscle
  { repsMin :: Int
  , repsMax :: Int
  , volMin :: Int
  , volMax :: Int
  }

instance decodeJsonMuscle :: DecodeJson Muscle where
  decodeJson json = do
    x <- decodeJson json
    repsMin <- x .: "muscleRepsMin"
    repsMax <- x .: "muscleRepsMax"
    volMin <- x .: "muscleVolMin"
    volMax <- x .: "muscleVolMax"
    pure $ Muscle {repsMin , repsMax, volMin, volMax}

instance encodeJsonMuscle :: EncodeJson Muscle where
  encodeJson (Muscle w) = do
    "workoutRepsMin" := w.repsMin
    ~> "workoutRepsMax" := w.repsMax
    ~> "workoutVolMin" := w.volMin
    ~> "workoutVolMax" := w.volMax
    ~> jsonEmptyObject
