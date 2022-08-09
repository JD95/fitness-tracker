module FitnessInfo
( module WorkoutSet,
  module Workout,
  module Muscle,
  module DbId,
  FitnessInfo(..)
)
where

import Data.Newtype
import WorkoutSet
import Workout
import Muscle
import Data.Map (Map)
import DbId

newtype FitnessInfo
  = FitnessInfo
  { workouts :: Map WorkoutId (Id Workout)
  , muscles :: Map MuscleId (Id Muscle)
  , primaryMuscles :: Map WorkoutId MuscleId
  , sets :: Map WorkoutSetId (Id WorkoutSet)
  , setsForWeek :: Array (Array (Id WorkoutSet))
  }

derive instance newtypeFitnessInfo :: Newtype FitnessInfo _
