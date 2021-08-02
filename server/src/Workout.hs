{-# LANGUAGE DeriveGeneric #-}

module Workout where

import Data.Aeson
import GHC.Generics

data Workout = Workout
  { workoutName :: String,
    workoutTargetMuscle :: String,
    workoutRepsMin :: Int,
    workoutRepsMax :: Int
  }
  deriving (Show, Generic)

instance ToJSON Workout

instance FromJSON Workout
