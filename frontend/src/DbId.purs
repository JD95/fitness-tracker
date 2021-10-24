module DbId where

import Prelude (pure, ($), bind)
import Data.Argonaut (class DecodeJson, class EncodeJson, jsonEmptyObject, decodeJson, (~>), (:=), (.:))

newtype Id a = Id 
  { id :: Int 
  , values :: a
  }

instance decodeJsonWorkout :: DecodeJson a => DecodeJson (Id a) where
  decodeJson json = do
    x <- decodeJson json
    id <- x .: "id"
    values <- x .: "values"
    pure $ Id {id, values}

instance encodeJsonId :: EncodeJson a => EncodeJson (Id a) where
  encodeJson (Id x) = do
    "id" := x.id 
    ~> "values" := x.values 
    ~> jsonEmptyObject
