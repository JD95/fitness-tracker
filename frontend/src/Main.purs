module Main where

import Prelude

import Control.Monad.Maybe.Trans
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (class DecodeJson, class EncodeJson, jsonEmptyObject, decodeJson, (~>), (:=), (.:))
import Data.Array as Array
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
import Halogen.HTML (button, p_, slot, slot_, div_, div, text, table_, tr, th_) as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (class_) as HH
import Halogen.HTML.Properties (InputType(..))
import Halogen.VDom.Driver (runUI)
import Web.HTML.Common (ClassName(..))

import PrimaryMuscle
import InputField as InputField
import RadioInput as RadioInput
import FitnessInfo
import WorkoutSelector as WorkoutSelector
import Utils (getJson, postJson)
import DbId

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body
data Action
  = Init
  | Submit
  | WorkoutSelected WorkoutSelector.Output

newtype Info
  = Info
    { fitnessInfo :: FitnessInfo
    , selectedWorkout :: Maybe WorkoutId 
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
          in HH.slot_ RadioInput.proxy intensitySlot (RadioInput.radio "intensityInput" opts) unit
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

    thisWeekSets =
      let (FitnessInfo info) = st.fitnessInfo
          sets = Array.fromFoldable $ Map.values info.sets
      in currentWeek st.timezoneOffset st.today sets

    renderWorkoutVolume :: H.ComponentHTML Action Slots m
    renderWorkoutVolume = HH.div_ (map go $ Map.toUnfoldable counts) where

      counts :: Map String Int
      counts = foldr (Map.unionWith (+)) Map.empty $ map countW thisWeekSets 

      countW (Id {values: WorkoutSet w}) =
        let (FitnessInfo info) = st.fitnessInfo
            result = do
              muscleId <- Map.lookup (WorkoutId w.workout) info.primaryMuscles 
              Id {values: Muscle m} <- Map.lookup muscleId info.muscles
              pure m.name
        in Map.singleton (maybe "unknown" identity result) 1

      go (Tuple muscle vol) = HH.p_ [HH.text $ muscle <> ": " <> show vol]

    renderSet (Id {id, values: WorkoutSet ws}) =
      let FitnessInfo info = st.fitnessInfo
          result = do
            Id {values: Workout w} <- Map.lookup (WorkoutId ws.workout) info.workouts
            pure $
              [ HH.th_ [ HH.text (displayDate st.timezoneOffset ws.date) ]
              , HH.th_ [ HH.text w.name ]
              , HH.th_ [ HH.text $ show ws.reps ]
              , HH.th_ [ HH.text $ show ws.weight ]
              , HH.th_ [ HH.text $ show ws.intensity ]
              ]
      in HH.tr [ HH.class_ (ClassName $ intensityColor ws.intensity) ] (maybe [] identity result)
      

    intensityColor 4 = "failSet"
    intensityColor 3 = "hardSet"
    intensityColor 2 = "goodSet"
    intensityColor 1 = "easySet"
    intensityColor _ = "noEffortSet"

  render (Error e) = HH.text e

  handleAction :: Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    Init -> do
      -- Get Time Info
      today <- H.liftEffect nowDateTime
      offset <- H.liftEffect $ JSDate.getTimezoneOffset =<< JSDate.now 

      -- Make API calls for data
      verifyWorkouts <- getJson "workouts"
      verifyPrimaryMuscles <- getJson "primary-muscles"
      verifyMuscles <- getJson "muscles"
      verifySets <- getJson "/sets"

      H.modify_ $ const $ either Error Full $ do

        -- Verify all the data from API is valid
        workouts <- verifyWorkouts
        muscles <- verifyMuscles 
        primaryMusclePairs <- verifyPrimaryMuscles
        sets <- verifySets

        -- Transform Values
        let primaryMuscles =
              Array.foldr
              (\(PrimaryMuscle {workout, muscle}) -> Map.insert (WorkoutId workout) (MuscleId muscle))
              Map.empty
              primaryMusclePairs

        pure $ Info
          { fitnessInfo: FitnessInfo
            { sets: mapFromIds WorkoutSetId sets
            , workouts: mapFromIds WorkoutId workouts
            , muscles: mapFromIds MuscleId muscles
            , primaryMuscles: primaryMuscles
            }
          , selectedWorkout: Nothing
          , today: maybe today identity $ DateTime.adjust (Minutes $ negate offset) today
          , timezoneOffset: Minutes $ negate offset
          }
    Submit -> do
      st <- H.get 
      let gather = runMaybeT $ do
            workoutId <- st.selectedWorkout
            -- Multiple input fields, so we need slots for them
            weight <- MaybeT $ join <$> H.request InputField.natProxy weightSlot InputField.GetValue
            reps <- MaybeT $ join <$> H.request InputField.natProxy repsSlot InputField.GetValue
            intensity <- MaybeT $ H.request RadioInput.proxy intensitySlot RadioInput.GetValue

            Time.Milliseconds date <- lift $ Date.unInstant <$> H.liftEffect now
            lift $ H.liftEffect $ log $ show date
            pure $ WorkoutSet
              { workout: workoutId
              , reps: natToInt reps
              , date: date / 1000.0
              , weight: natToInt weight
              , intensity: intensity
              }
      gather >>= case _ of
        Just ws -> do
          -- TODO return new id from POST /sets 
          postJson ws "/sets"
          -- Parse the new id
          let newSetId = undefined
          H.modify_ $ case _ of
            Full (Info ss) ->
              let FitnessInfo info = ss.fitnessInfo
              in Full $ Info ss
                   { fitnessInfo = info
                     { sets = Map.insert (WorkoutSetId newSetId) (Id newSetId ws) info.sets
                     }
                   }
            prev -> prev
        Nothing -> pure unit 
    WorkoutSelected (WorkoutSelector.Selection w) -> do
      H.modify_ $ \st -> case st of
        Full (Info i) -> Full $ Info i
          { selectedWorkout = Just w
          }
        _ -> st
      
recommendedWeights :: forall m. Info -> H.ComponentHTML Action Slots m
recommendedWeights (Info {selectedWorkout, fitnessInfo: FitnessInfo info}) = HH.text $
  case selectedWorkout of
    Just workoutId -> 
      -- lookup workout
      case Map.lookup workoutId info.workouts of
        Just (Id w) ->
          let matchWorkout = (\(Id {values: WorkoutSet s}) -> s.workout == w.id)
          in case head $ filter matchWorkout $ Array.fromFoldable $ Map.values info.sets of
               (Just (Id {values: WorkoutSet x})) -> "Weight for Previous Set: " <> show x.weight
               Nothing -> ""
        Nothing -> ""
    Nothing -> ""

recommendedRepRange :: forall m. Info -> H.ComponentHTML Action Slots m
recommendedRepRange (Info {selectedWorkout, fitnessInfo: FitnessInfo info})
  = HH.text $
    let result = do
          workoutId <- selectedWorkout
          muscleId <- Map.lookup workoutId info.primaryMuscles
          Id {values: Muscle m} <- Map.lookup muscleId info.muscles
          pure $ "Recommended Rep Range: (" <> show m.repsMin <> "-" <> show m.repsMax <> ")"
    in maybe "" identity result

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

workoutSetDateTime :: Minutes -> Id WorkoutSet -> Maybe DateTime
workoutSetDateTime offset (Id {values: WorkoutSet ws}) = unixEpochToDateTime offset ws.date

currentWeek :: Minutes -> DateTime -> Array (Id WorkoutSet) -> Array (Id WorkoutSet)
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
      
