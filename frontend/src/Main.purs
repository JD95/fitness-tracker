module Main where

import Prelude (Unit, Void, bind, const, map, pure, show, unit, ($), (<>), (<$>))

import Data.Argonaut (class DecodeJson, decodeJson, (.:))
import Data.Either (Either(..), either)
import Data.Formatter.DateTime (FormatterCommand(..), format, unformat)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (slot_, div_, div, text, select_, option_, table_, tr_, th_) as HH
import Halogen.HTML.Properties (class_) as HH
import Halogen.VDom.Driver (runUI)
import Web.HTML.Common (ClassName(..))

import WorkoutSelector as WorkoutSelector
import Utils (getJson)

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

newtype Info = Info {sets :: Array WorkoutSet, workouts :: Array String}

type Slots = (workoutSelector :: forall q. H.Slot q Void Unit)

data State
  = Empty
  | Full Info
  | Error String

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
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

  render :: State -> H.ComponentHTML Action Slots m
  render Empty = HH.text "No data yet"
  render (Full (Info state)) = HH.div [ HH.class_ (ClassName "content") ]
    [ HH.div_
      [ HH.slot_ WorkoutSelector.proxy unit WorkoutSelector.comp unit 
      , HH.table_ (renderSet <$> state.sets)
      ]
    ]

    where

    renderWorkoutSelector ws = HH.select_ (map (\w -> HH.option_ [HH.text w]) ws)

    renderSet (WorkoutSet ws) = HH.tr_
      [ HH.th_ [ HH.text dateTxt ]
      , HH.th_ [ HH.text ws.name ]
      , HH.th_ [ HH.text $ show ws.reps ]
      , HH.th_ [ HH.text $ show ws.weight ]
      ]
      where
      dateTxt = case unformat dateReadFormat ws.date of
        Right t -> format dateWriteFormat t
        Left _ -> "Format failed " <> ws.date
  render (Error e) = HH.text e

  handleAction :: Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    Init -> do
      verifySets <- getJson "/sets"
      verifyWorkouts <- getJson "workouts"
      H.modify_ $ const $ either Error Full $ do
        sets <- verifySets
        workouts <- verifyWorkouts
        pure $ Info {sets: sets, workouts: workouts}


dateReadFormat :: List FormatterCommand
dateReadFormat = List.fromFoldable
  [ YearFull
  , Placeholder "-"
  , MonthTwoDigits
  , Placeholder "-"
  , DayOfMonthTwoDigits
  , Placeholder "T"
  , Hours24
  , Placeholder ":"
  , MinutesTwoDigits
  , Placeholder ":"
  , SecondsTwoDigits
  , Placeholder "Z"
  ]

dateWriteFormat :: List FormatterCommand
dateWriteFormat = List.fromFoldable
  [ DayOfMonthTwoDigits
  , Placeholder "-"
  , MonthTwoDigits
  , Placeholder "-"
  , YearFull
  ]
