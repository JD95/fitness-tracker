{-# LANGUAGE DeriveGeneric #-}

module DbId where

import Data.Aeson
import GHC.Generics

data Id a = Id {id :: Int, values :: a}
  deriving (Show, Generic)

instance ToJSON a => ToJSON (Id a)

instance FromJSON a => FromJSON (Id a)
