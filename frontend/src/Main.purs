module Main where

import Prelude (Unit, Void, discard, bind, join, const, pure, show, unit, ($), (<>), (<$>), (>>=))

import Effect.Console (log)
import Data.Array(cons)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Maybe.Trans
import Data.Natural (Natural, natToInt)
import Data.Argonaut (class DecodeJson, class EncodeJson, jsonEmptyObject, decodeJson, (~>), (:=), (.:))
import Data.Either (Either(..), either)
import Data.Formatter.DateTime (FormatterCommand(..), format, unformat)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (button, p_, slot_, div_, div, text, table_, tr_, th_) as HH
import Halogen.HTML.Properties (class_) as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Web.HTML.Common (ClassName(..))
import Effect.Now (nowDateTime)
import Data.DateTime as Date
import Data.Time.Duration as Time 

import InputField as InputField 
import WorkoutSelector as WorkoutSelector
import Utils (getJson, postJson)

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

instance encodeJsonWorkoutSet :: EncodeJson WorkoutSet where
  encodeJson (WorkoutSet set) = do
    "setWorkout" := set.name
    ~> "setReps" := set.reps
    ~> "setDate" := set.date
    ~> "setWeight" := set.weight
    ~> jsonEmptyObject

data Action
  = Init
  | Submit

newtype Info = Info {sets :: Array WorkoutSet}

type Slots =
  ( workoutSelector :: H.Slot WorkoutSelector.Query Void Unit
  , natField :: H.Slot (InputField.Query Natural) Void Int
  )

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
      [ HH.p_
        [ HH.text "Workout: "
        , HH.slot_ WorkoutSelector.proxy unit WorkoutSelector.comp unit
        ]
      , HH.p_
        [ HH.text "Weight: "
        , HH.slot_ InputField.natProxy 0 InputField.nat unit
        ]
      , HH.p_
        [ HH.text "Reps: "
        , HH.slot_ InputField.natProxy 1 InputField.nat unit
        ]
      , HH.p_
        [ HH.button [HE.onClick (const Submit)]
          [ HH.text "submit" ]
        ]
      , HH.table_ (renderSet <$> state.sets)
      ]
    ]

    where

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
      H.modify_ $ const $ either Error Full $ do
        sets <- verifySets
        pure $ Info {sets: sets }
    Submit -> do
      let gather = runMaybeT $ do
            workout <- MaybeT $ H.request WorkoutSelector.proxy unit WorkoutSelector.GetValue
            reps <- MaybeT $ join <$> H.request InputField.natProxy 0 InputField.GetValue
            weight <- MaybeT $ join <$> H.request InputField.natProxy 1 InputField.GetValue
            date <- format dateReadFormat <$> (MaybeT $ do
                                                 t <- H.liftEffect nowDateTime
                                                 pure $ Date.adjust (Time.negateDuration (Time.Hours 7.0)) t
                                              )
            lift $ H.liftEffect $ log $ show date
            pure $ WorkoutSet
              { name: workout
              , reps: natToInt reps
              , date: date
              , weight: natToInt weight
              }
      gather >>= case _ of
        Just ws -> do
          postJson ws "/sets"
          H.modify_ $ \st -> case st of
            Full (Info ss) -> Full (Info ss {sets = cons ws ss.sets})
            _ -> st
        Nothing -> pure unit 

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
