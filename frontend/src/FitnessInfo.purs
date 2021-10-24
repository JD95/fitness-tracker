module FitnessInfo where

import Workout (Workout, WorkoutId)
import Muscle (Muscle, MuscleId)
import Data.Map (Map)
import DbId (Id)

newtype FitnessInfo
  = FitnessInfo
  { workouts :: Map WorkoutId (Id Workout)
  , muscles :: Map MuscleId (Id Muscle)
  , primaryMuscles :: Map WorkoutId MuscleId
  }
