module DbId where

import Data.Ord
import Data.Array as Array
import Data.Map as Map
import Data.Map (Map)
import Prelude (pure, ($), bind)
import Data.Argonaut (class DecodeJson, class EncodeJson, jsonEmptyObject, decodeJson, (~>), (:=), (.:))
import Data.Newtype

newtype Id a = Id
  { id :: Int
  , values :: a
  }

derive instance newtypeId :: Newtype (Id a) _

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

mapFromIds :: forall key value. Ord key => (Int -> key) -> Array (Id value) -> Map key (Id value)
mapFromIds toKey = Array.foldr (\(Id x) -> Map.insert (toKey x.id) (Id x)) Map.empty
