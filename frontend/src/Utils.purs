module Utils where

import Prelude (Unit, map, void, bind, pure, ($))

import Data.Maybe (Maybe(..))
import Affjax as AX
import Affjax.RequestBody as AX
import Affjax.ResponseFormat as AXRF
import Data.Argonaut (Json, class DecodeJson, class EncodeJson, encodeJson, decodeJson, jsonParser, JsonDecodeError)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H

getJson ::
  forall a state action slots output m.
  DecodeJson a =>
  MonadAff m =>
  String ->
  H.HalogenM state action slots output m (Either String a)
getJson endpoint = do 
    response <- H.liftAff $ AX.request $ AX.defaultRequest
      { url = endpoint 
      , method = Left GET
      , responseFormat = AXRF.string
      }
    pure $ case response of
      Right r -> case jsonParser r.body of
        Right json -> case decodeJson json of
          Right x -> Right x
          Left _ -> Left "Problem decoding json" 
        Left _ -> Left "Problem parsing json"
      Left _ -> Left "Problem making request" 

postJson_ ::
  forall a state action slots output m.
  EncodeJson a =>
  MonadAff m =>
  a ->
  String ->
  H.HalogenM state action slots output m Unit
postJson_ val endpoint = void $ H.liftAff $ AX.request $ AX.defaultRequest
  { url = endpoint
  , method = Left POST
  , content = Just $ AX.Json (encodeJson val)
  }

data RequestError
  = RequestError AX.Error
  | DecodingError JsonDecodeError

postJson ::
  forall a b state action slots output m.
  EncodeJson a =>
  DecodeJson b =>
  MonadAff m =>
  a ->
  String ->
  H.HalogenM state action slots output m (Either RequestError b)
postJson val endpoint = H.liftAff $ do
  response <- AX.request $ AX.defaultRequest
    { url = endpoint
    , method = Left POST
    , content = Just $ AX.Json (encodeJson val)
    , responseFormat = AXRF.json
    }
  pure $ case response of
    Right r -> case decodeJson r.body of
      Right value -> Right value
      Left e -> Left $ DecodingError e
    Left e -> Left $ RequestError e
