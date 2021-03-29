module Main where

import Prelude

import Data.Unit
import Data.Maybe
import Data.Either
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Affjax.RequestHeader
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Data.HTTP.Method (Method(..))
import Halogen.VDom.Driver (runUI)
import Data.Argonaut

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

newtype WorkoutSet = WorkoutSet
  { name :: String
  , reps :: Int
  , date :: String
  , weight :: Int
  }

instance decodeJsonWorkoutSet :: DecodeJson WorkoutSet where
  decodeJson json = do
    x <- decodeJson json
    name <- x .: "setWorkout"
    reps <- x .: "setReps"
    date <- x .: "setDate"
    weight <- x .: "setWeight"
    pure $ WorkoutSet { name, reps, date, weight }

data Action = Init 
data State
  = Empty
  | Full (Array WorkoutSet)
  | Error String

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval {
      handleAction = handleAction,
      initialize = Just Init
      }
    }
  where
  initialState :: forall a. a -> State 
  initialState _ = Empty 

  render :: State -> H.ComponentHTML Action () m
  render Empty = HH.text "No data yet"
  render (Full state) = HH.div_ (map renderSet state) where
    renderSet (WorkoutSet ws) = HH.div_ [HH.text $ ws.name]
  render (Error e) = HH.text e

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    Init -> do
      response <- H.liftAff $ AX.request $ AX.defaultRequest {
            url = "/sets",
            method = Left GET,
            responseFormat = AXRF.string
            }
      let result = case response of
            Right r -> case jsonParser r.body of
              Right json -> case decodeJson json of
                Right sets -> Full sets
                Left _ -> Error "Problem decoding json" 
              Left _ -> Error "Problem parsing json"
            Left _ -> Error "Problem making request" 
      H.modify_ $ const result 
