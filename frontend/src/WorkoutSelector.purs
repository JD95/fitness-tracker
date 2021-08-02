module WorkoutSelector (Query(..), Output(..), Workout(..), proxy, comp) where

import Prelude (Unit, pure, unit, bind, const, map, discard, ($), (>>=), (==))

import Type.Proxy (Proxy(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML (text, select, option_) as HH
import Halogen.HTML.Events as HE
import Data.Array (head, filter)
import Data.Argonaut (class DecodeJson, class EncodeJson, jsonEmptyObject, decodeJson, (~>), (:=), (.:))

import Utils (getJson)

data Action
  = Init
  | Select String

data State
  = Empty
  | Full (Array Workout) Workout 
  | Error String

data Query a
  = GetValue (Workout -> a)

data Output
  = Selection
    
newtype Workout = Workout
  { name :: String
  , targetMuscle :: String
  , repsMin :: Int
  , repsMax :: Int
  }

instance decodeJsonWorkout :: DecodeJson Workout where
  decodeJson json = do
    x <- decodeJson json
    name <- x .: "workoutName"
    targetMuscle <- x .: "workoutTargetMuscle"
    repsMin <- x .: "workoutRepsMin"
    repsMax <- x .: "workoutRepsMax"
    pure $ Workout { name, targetMuscle, repsMin , repsMax}

instance encodeJsonWorkout :: EncodeJson Workout where
  encodeJson (Workout w) = do
    "workoutName" := w.name
    ~> "workoutTargetMuscle" := w.targetMuscle
    ~> "workoutRepsMin" := w.repsMin
    ~> "workoutRepsMax" := w.repsMax
    ~> jsonEmptyObject


proxy :: Proxy "workoutSelector"
proxy = Proxy

comp :: forall input output m. MonadAff m => H.Component Query input Output m
comp =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction 
      , initialize = Just Init
      , handleQuery = handleQuery
      }
    }
  where

  initialState :: forall a. a -> State 
  initialState _ = Empty 

  render :: State -> H.ComponentHTML Action () m
  render Empty = HH.text "No data yet"
  render (Full options _) = HH.select [ HE.onValueChange Select]
                            (map (\(Workout w) -> HH.option_ [HH.text w.name]) options)
  render (Error e) = HH.text e

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of
    Init -> do
      verifyWorkouts <- getJson "workouts"
      case verifyWorkouts of 
        Right ws -> case head ws of
          Just x -> H.modify_ $ const $ Full ws x
          Nothing -> pure unit
        Left e -> H.modify_ $ const $ Error e 
    Select x -> do
      H.modify_ $ case _ of
        Empty -> Empty
        Full ws wo -> case head $ filter (\(Workout w) -> w.name == x) ws of
          Just wo' -> Full ws wo'
          Nothing -> Full ws wo
        Error e -> Error e
      -- Notify parent that selection has changed
      H.raise Selection

  handleQuery :: forall x. Query x -> H.HalogenM State Action () Output m (Maybe x)
  handleQuery = case _ of
      GetValue reply -> do
        H.get >>= case _ of
          Full _ selected -> pure $ Just $ reply selected
          _ -> pure Nothing


