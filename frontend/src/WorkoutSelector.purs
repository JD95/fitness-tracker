module WorkoutSelector (proxy, comp) where

import Prelude (Unit, bind, const, map, ($)) 

import Type.Proxy 
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML (text, select_, option_) as HH

import Utils (getJson)

data Action = Init

data State
  = Empty
  | Full (Array String)
  | Error String

proxy :: Proxy "workoutSelector"
proxy = Proxy

comp :: forall query input output m. MonadAff m => H.Component query input output m
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
  render (Full state) = HH.select_ (map (\w -> HH.option_ [HH.text w]) state)
  render (Error e) = HH.text e

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    Init -> do
      verifyWorkouts <- getJson "workouts"
      H.modify_ $ const $ either Error Full verifyWorkouts

