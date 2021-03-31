module Utils where

import Prelude (Unit, bind, const, map, pure, show, unit, ($), (<>), (<$>))

import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Data.Argonaut (class DecodeJson, decodeJson, jsonParser, (.:))
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H

getJson :: forall a state action slots output m.
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
