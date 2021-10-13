module Main where

import Prelude

import Control.Monad.Maybe.Trans
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (class DecodeJson, class EncodeJson, jsonEmptyObject, decodeJson, (~>), (:=), (.:))
import Data.Array (length, cons, filter, head, takeWhile)
import Data.DateTime (DateTime, date, weekday)
import Data.DateTime (adjust) as DateTime
import Data.DateTime.Instant (Instant, instant, unInstant) as Date
import Data.Foldable (foldr)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Either (Either(..), either)
import Data.Enum (fromEnum)
import Data.Formatter.DateTime (format, FormatterCommand(DayOfMonthTwoDigits, Placeholder, MonthTwoDigits, YearFull))
import Data.Int (toNumber)
import Data.JSDate (JSDate)
import Data.JSDate as JSDate
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map 
import Data.Maybe (Maybe(..), maybe)
import Data.Natural (Natural, natToInt)
import Data.Time.Duration (Minutes(..), Days(..))
import Data.Time.Duration as Time
import Data.Tuple
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Effect.Now (now, nowDateTime)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (button, p_, slot, slot_, div_, div, text, table_, tr_, th_) as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (class_) as HH
import Halogen.HTML.Properties (InputType(..))
import Halogen.VDom.Driver (runUI)
import Web.HTML.Common (ClassName(..))

import InputField as InputField
import RadioInput as RadioInput 
import WorkoutSelector (Workout(..))
import WorkoutSelector as WorkoutSelector
import Utils (getJson, postJson)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

newtype WorkoutSet = WorkoutSet
  { name :: String
  , reps :: Int
  , date :: Number 
  , weight :: Int
  , intensity :: Int
  }

instance decodeJsonWorkoutSet :: DecodeJson WorkoutSet where
  decodeJson json = do
    x <- decodeJson json
    name <- x .: "setWorkout"
    reps <- x .: "setReps"
    date <- x .: "setDate"
    weight <- x .: "setWeight"
    intensity <- x .: "setIntensity"
    pure $ WorkoutSet { name, reps, date, weight, intensity }

instance encodeJsonWorkoutSet :: EncodeJson WorkoutSet where
  encodeJson (WorkoutSet set) = do
    "setWorkout" := set.name
    ~> "setReps" := set.reps
    ~> "setDate" := set.date
    ~> "setWeight" := set.weight
    ~> "setIntensity" := set.intensity
    ~> jsonEmptyObject

data Action
  = Init
  | Submit
  | WorkoutSelected WorkoutSelector.Output

newtype Info
  = Info
    { sets :: Array WorkoutSet
    , selectedWorkout :: Maybe Workout
    , targetMuscles :: Map String String 
    , today :: DateTime
    , timezoneOffset :: Minutes 
    }

type Slots =
  ( workoutSelector :: H.Slot WorkoutSelector.Query WorkoutSelector.Output Unit
  , natField :: H.Slot (InputField.Query Natural) Void Int
  , radioInput :: H.Slot (RadioInput.Query Int) Void Int
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

  weightSlot = 0
  repsSlot = 1
  intensitySlot = 2

  render :: State -> H.ComponentHTML Action Slots m
  render Empty = HH.text "No data yet"
  render (Full state@(Info st)) = HH.div [ HH.class_ (ClassName "content") ]
    [ HH.div_
      [ HH.p_
        [ HH.text "Workout: "
          -- The WorkoutSelector component actually gives output
          -- so we'll wrap what it gives so this component can handle it
        , HH.slot WorkoutSelector.proxy unit WorkoutSelector.comp unit WorkoutSelected
        ]
      , HH.p_
        [ HH.text "Weight: "
        , HH.slot_ InputField.natProxy weightSlot InputField.nat unit
        ]
      , HH.p_
        [ recommendedWeights state
        ]
      , HH.p_
        [ HH.text "Reps: "
        , HH.slot_ InputField.natProxy repsSlot InputField.nat unit
        ]
      , HH.p_
        [ recommendedRepRange state 
        ]
      , HH.p_
        [ HH.text "Intensity: "
        , let opts = ["No Effort", "Easy", "Good", "Hard", "Fail"]
          in HH.slot_ RadioInput.proxy intensitySlot (RadioInput.radio opts) unit
        ]
      , HH.p_
        [ HH.button [HE.onClick (const Submit)]
          [ HH.text "submit" ]
        ]
      , renderWorkoutVolume 
      , HH.table_ (renderSet <$> thisWeekSets)
      ]
    ]

    where

    thisWeekSets = currentWeek st.timezoneOffset st.today st.sets

    renderWorkoutVolume :: H.ComponentHTML Action Slots m
    renderWorkoutVolume = HH.div_ (map go $ Map.toUnfoldable counts) where

      counts :: Map String Int
      counts = foldr (Map.unionWith (+)) Map.empty $ map countW thisWeekSets 

      countW (WorkoutSet w) =
        let m = case Map.lookup w.name st.targetMuscles of
              Just muscle -> muscle
              Nothing -> "unknown" 
        in Map.singleton m 1

      go (Tuple muscle vol) = HH.p_ [HH.text $ muscle <> ": " <> show vol]

    renderSet (WorkoutSet ws) = HH.tr_
      [ HH.th_ [ HH.text (displayDate st.timezoneOffset ws.date) ]
      , HH.th_ [ HH.text ws.name ]
      , HH.th_ [ HH.text $ show ws.reps ]
      , HH.th_ [ HH.text $ show ws.weight ]
      , HH.th_ [ HH.text $ show ws.intensity ]
      ]

  render (Error e) = HH.text e

  handleAction :: Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    Init -> do
      workout <- H.request WorkoutSelector.proxy unit WorkoutSelector.GetValue
      verifyWorkouts <- getJson "workouts"
      let targetMuscles = case verifyWorkouts of
            Right xs -> 
              Map.fromFoldable $ map (\(Workout x) -> (Tuple x.name x.targetMuscle)) (xs :: Array Workout)
            Left e -> Map.empty 
      verifySets <- getJson "/sets"
      t <- H.liftEffect nowDateTime
      o <- H.liftEffect $ JSDate.getTimezoneOffset =<< JSDate.now 
      H.modify_ $ const $ either Error Full $ do
        sets <- verifySets
        pure $ Info
          { sets: sets
          , selectedWorkout: workout
          , targetMuscles: targetMuscles 
          , today: maybe t identity $ DateTime.adjust (Minutes $ negate o) t
          , timezoneOffset: Minutes $ negate o
          }
    Submit -> do
      let gather = runMaybeT $ do
            -- There's only one workout selector, so unit
            Workout workout <- MaybeT $ H.request WorkoutSelector.proxy unit WorkoutSelector.GetValue

            -- Multiple input fields, so we need slots for them
            weight <- MaybeT $ join <$> H.request InputField.natProxy weightSlot InputField.GetValue
            reps <- MaybeT $ join <$> H.request InputField.natProxy repsSlot InputField.GetValue
            intensity <- MaybeT $ H.request RadioInput.proxy intensitySlot RadioInput.GetValue

            Time.Milliseconds date <- lift $ Date.unInstant <$> H.liftEffect now
            lift $ H.liftEffect $ log $ show date
            pure $ WorkoutSet
              { name: workout.name
              , reps: natToInt reps
              , date: date / 1000.0
              , weight: natToInt weight
              , intensity: intensity
              }
      gather >>= case _ of
        Just ws -> do
          postJson ws "/sets"
          H.modify_ $ \st -> case st of
            Full (Info ss) -> Full (Info ss {sets = cons ws ss.sets})
            _ -> st
        Nothing -> pure unit 
    WorkoutSelected _ -> do
      w <- H.request WorkoutSelector.proxy unit WorkoutSelector.GetValue
      H.modify_ $ \st -> case st of
        Full (Info i) -> Full $ Info i
          { selectedWorkout = w
          }
        _ -> st
      
recommendedWeights :: forall m. Info -> H.ComponentHTML Action Slots m
recommendedWeights (Info info) = HH.text $
  case info.selectedWorkout of
    Just (Workout w) ->
      case head $ filter (\(WorkoutSet s) -> s.name == w.name) $ info.sets of
        (Just (WorkoutSet x)) -> "Weight for Previous Set: " <> show x.weight
        Nothing -> ""
    Nothing -> ""

recommendedRepRange :: forall m. Info -> H.ComponentHTML Action Slots m
recommendedRepRange (Info info) = HH.text $  
  case info.selectedWorkout of
    Just (Workout w) -> "Recommended Rep Range: (" <> show w.repsMin <> "-" <> show w.repsMax <> ")"
    Nothing -> ""

dateWriteFormat :: List FormatterCommand
dateWriteFormat = List.fromFoldable
  [ DayOfMonthTwoDigits
  , Placeholder "-"
  , MonthTwoDigits
  , Placeholder "-"
  , YearFull
  ]

unixEpochToDateTime :: Minutes -> Number -> Maybe DateTime 
unixEpochToDateTime offset date
  = DateTime.adjust offset
    =<< JSDate.toDateTime
    =<< (JSDate.fromInstant <$> Date.instant (Time.Milliseconds (date * 1000.0)))

displayDate :: Minutes -> Number -> String
displayDate offset date = case unixEpochToDateTime offset date of
  Just dt -> format dateWriteFormat dt 
  Nothing -> "Failed to parse date"

workoutSetDateTime :: Minutes -> WorkoutSet -> Maybe DateTime
workoutSetDateTime offset (WorkoutSet ws) = unixEpochToDateTime offset ws.date

currentWeek :: Minutes -> DateTime -> Array WorkoutSet -> Array WorkoutSet
-- Weeks begin on Sunday so take everything until
-- the previous Saturday
currentWeek offset today sets =
  case DateTime.adjust days today of
    Just d -> takeWhile (after d) sets
    Nothing -> sets

  where

    days = Days $ toNumber $ negate $ (fromEnum (weekday $ date today) + 1) `mod` 7

    after d s = case workoutSetDateTime offset s of
      Just t -> d <= t
      Nothing -> false
      
