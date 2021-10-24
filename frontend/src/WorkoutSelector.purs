module WorkoutSelector (Query(..), Output(..), proxy, comp) where

import Prelude (Unit, pure, bind, const, map, discard, ($), (>>=), (==))

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
  , selected :: Maybe WorkoutId
  }

data Query a
  = GetValue (WorkoutId -> a)

data Output
  = Selection

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
      , handleQuery = handleQuery
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
        names = sort $ Array.fromFoldable $ map (\(Id {values: Workout w}) -> w.name) $ Map.values info.workouts
        items = map (\name -> HH.option_ [HH.text name]) names 
    in HH.select [ HE.onValueChange Select ] items

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of
    Init -> do
      verifyWorkouts <- getJson "workouts"
      case verifyWorkouts of 
        Right ws ->
          let workouts = Array.foldr (\(Id x) -> Map.insert (WorkoutId x.id) x.values) Map.empty ws
              selected = map (\(Id x) -> WorkoutId x.id) $ Array.head ws 
          in H.modify_ $ const $ Full $ Info {workouts, selected}
        Left e -> H.modify_ $ const $ Error e 
    Select x -> do
      H.modify_ $ case _ of
        Empty -> Empty
        Full (Info info) ->
          let matchName (Id {values: Workout w}) = w.name == x
              selection = Array.head $ filter matchName $ Array.fromFoldable $ Map.values info.workouts 
          in Full (Info info
                   { selected = case selection of
                       Just (Id w) -> Just $ WorkoutId w.id
                       Nothing -> info.selected
                   })
        Error e -> Error e

      -- Notify parent that selection has changed
      H.raise Selection

  handleQuery :: forall x. Query x -> H.HalogenM State Action () Output m (Maybe x)
  handleQuery = case _ of
      GetValue reply -> do
        H.get >>= case _ of
          Full (Info {selected: Just x}) -> pure $ Just $ reply x
          _ -> pure Nothing


