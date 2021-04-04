module WorkoutSelector (Query(..), proxy, comp) where

import Prelude (Unit, pure, unit, bind, const, map, ($), (>>=))

import Type.Proxy (Proxy(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML (text, select, option_) as HH
import Halogen.HTML.Events as HE
import Data.Array (head)

import Utils (getJson)

data Action
  = Init
  | Select String

data State
  = Empty
  | Full (Array String) String 
  | Error String

data Query a
  = GetValue (String -> a)

proxy :: Proxy "workoutSelector"
proxy = Proxy

comp :: forall input output m. MonadAff m => H.Component Query input output m
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
  render (Full options _) = HH.select [ HE.onValueChange Select] (map (\w -> HH.option_ [HH.text w]) options)
  render (Error e) = HH.text e

  handleAction :: Action -> H.HalogenM State Action () output m Unit
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
        Full ws _ -> Full ws x
        Error e -> Error e

  handleQuery :: forall x. Query x -> H.HalogenM State Action () output m (Maybe x)
  handleQuery = case _ of
      GetValue reply -> do
        H.get >>= case _ of
          Full _ selected -> pure $ Just $ reply $ selected
          _ -> pure Nothing


