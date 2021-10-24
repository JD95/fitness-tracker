module WorkoutSelector (Query(..), Output(..), proxy, comp) where

import Prelude (Unit, unit, pure, bind, const, map, discard, ($), (>>=), (==))

import Data.Map (Map)
import Data.Map as Map 
import Type.Proxy (Proxy(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML (text, select, option_) as HH
import Halogen.HTML.Events as HE
import Data.Array as Array
import Data.Array (filter, sort)
import DbId

import Workout (Workout(..), WorkoutId(..))
import Utils (getJson)

data Action
  = Init
  | Select String

data State
  = Empty
  | Error String
  | Full Info

newtype Info
  = Info
  { workouts :: Map WorkoutId (Id Workout) 
  , workoutNames :: Map String WorkoutId
  }

data Query a

data Output
  = Selection WorkoutId

proxy :: Proxy "workoutSelector"
proxy = Proxy

comp :: forall input m. MonadAff m => H.Component Query input Output m
comp =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction 
      , initialize = Just Init
      }
    }
  where

  initialState :: forall a. a -> State 
  initialState _ = Empty 

  render :: State -> H.ComponentHTML Action () m
  render Empty = HH.text "No data yet"
  render (Error e) = HH.text e
  render (Full (Info info)) =
    let names :: Array String
        names = sort
                $ Array.fromFoldable
                $ map (\(Id {values: Workout w}) -> w.name)
                $ Map.values info.workouts
        items = map (\name -> HH.option_ [HH.text name]) names 
    in HH.select [ HE.onValueChange Select ] items

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of
    Init -> do
      getJson "workouts" >>= case _ of
        Right (ws :: Array (Id Workout)) ->
          let workouts :: Map WorkoutId (Id Workout)
              workouts =
                let toWorkoutEntry (Id x) = Map.insert (WorkoutId x.id) (Id x)
                in Array.foldr toWorkoutEntry Map.empty ws
              workoutNames :: Map String WorkoutId
              workoutNames =
                let toNameEntry (Id {id: i, values: Workout w}) = Map.insert w.name (WorkoutId i)
                in Array.foldr toNameEntry Map.empty ws
          in H.modify_ $ const $ Full $ Info {workouts, workoutNames}
        Left e -> H.modify_ $ const $ Error e 
    Select x -> do
      H.get >>= case _ of
        Error e -> pure unit 
        Empty -> pure unit 
        Full (Info info) ->
          case Map.lookup x info.workoutNames of
            -- Notify parent that selection has changed
            Just w -> H.raise (Selection w)
            Nothing -> pure unit 
