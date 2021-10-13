{-# LANGUAGE DeriveGeneric #-}

module WorkoutSet where

import Data.Aeson
import Data.Time.Clock
import GHC.Generics

data WorkoutSet = MkWorkoutSet
  { setWorkout :: String,
    setReps :: Int,
    setDate :: Double,
    setWeight :: Int,
    setIntensity :: Int
  }
  deriving (Show, Generic)

instance ToJSON WorkoutSet

instance FromJSON WorkoutSet
