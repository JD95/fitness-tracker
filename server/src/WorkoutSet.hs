{-# LANGUAGE DeriveGeneric #-}

module WorkoutSet where

import Data.Aeson
import Data.Time.Clock
import GHC.Generics
import Time

data WorkoutSet = MkWorkoutSet
  { setWorkout :: Int,
    setReps :: Int,
    setDate :: Time,
    setWeight :: Int,
    setIntensity :: Int
  }
  deriving (Show, Generic)

instance ToJSON WorkoutSet

instance FromJSON WorkoutSet
