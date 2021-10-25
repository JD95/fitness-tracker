{-# LANGUAGE DeriveGeneric #-}

module PrimaryMuscle where

import Data.Aeson
import GHC.Generics

data PrimaryMuscle = PrimaryMuscle
  { primaryMuscleWorkout :: Int,
    primaryMuscleMuscle :: Int
  }
  deriving (Show, Generic)

instance ToJSON PrimaryMuscle

instance FromJSON PrimaryMuscle
