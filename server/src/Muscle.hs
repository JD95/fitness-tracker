{-# LANGUAGE DeriveGeneric #-}

module Muscle where

import Data.Aeson
import GHC.Generics

data Muscle = Muscle
  { muscleName :: String,
    muscleRepsMin :: Int,
    muscleRepsMax :: Int,
    muscleVolMin :: Int,
    muscleVolMax :: Int
  }
  deriving (Show, Generic)

instance ToJSON Muscle

instance FromJSON Muscle
