module Main where

import Prelude
  (Unit, Void, bind, const, discard,
   identity, join, map, negate,
   pure, show, unit, ($), (*), (+),
   (/), (<$>), (<>), (=<<), (/=),
   (==), (>>=), (<<<))

import Data.List as List
import Control.Monad.Maybe.Trans
import Control.Monad.Trans.Class (lift)
import Data.Array (filter, head)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEArray
import Data.Array as Array
import Data.DateTime (DateTime, date, day)
import Data.DateTime (adjust) as DateTime
import Data.DateTime.Instant (instant, unInstant) as Date
import Data.Either (Either(..), either)
import Data.Foldable (foldr, intercalate)
import Data.Formatter.DateTime
  (format, FormatterCommand(DayOfMonthTwoDigits, Placeholder,
                            MonthTwoDigits, YearFull))
import Data.JSDate as JSDate
import Data.List (List)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Natural (Natural, natToInt)
import Data.Time.Duration (Minutes(..))
import Data.Time.Duration as Time
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Effect.Now (now, nowDateTime)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML
  (button, p_, slot, slot_, div_, div,
   text, table, table_, tr, tr_, th, th_) as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (class_) as HH
import Halogen.VDom.Driver (runUI)
import Web.HTML.Common (ClassName(..))

import PrimaryMuscle (PrimaryMuscle(..))
import InputField as InputField
import RadioInput as RadioInput
import FitnessInfo
  (FitnessInfo(..), Id(..), Muscle(..), MuscleId(..),
   Workout(..), WorkoutId(..), WorkoutSet(..),
   WorkoutSetId(..), mapFromIds)
import WorkoutSelector as WorkoutSelector
import Utils (getJson, postJson)

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
      , HH.p_ [recommendedWeights state]
      , HH.p_
        [ HH.text "Reps: "
        , HH.slot_ InputField.natProxy repsSlot InputField.nat unit
        ]
      , HH.p_ [recommendedRepRange state]
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
      , thisWeeksSetsGrouped
      ]
    , workoutSetHistory
    ]

    where

    thisWeeksSetsGrouped =
      HH.table [ HH.class_ (ClassName "pastWorkoutTable") ] $
        map mkSetGroup $
        Array.concat $
        map (Array.groupBy (\(WorkoutSet a) (WorkoutSet b) -> a.workout == b.workout)
               <<< NEArray.toArray) $
        Array.groupBy (\(WorkoutSet a) (WorkoutSet b) -> sameDay st.timezoneOffset a.date b.date) $
        map (\(Id {values: x}) -> x) $
        thisWeekSets

      where

      FitnessInfo info = st.fitnessInfo

      nameLookup x = do
        Id {values: Workout w} <- Map.lookup x info.workouts
        pure w.name

      mkSetGroup sets = setGroup name (NEArray.toArray sets) where
        WorkoutSet ws = NEArray.head sets
        name = maybe "Unknown" identity (nameLookup (WorkoutId ws.workout))

    workoutSetHistory = HH.div_ $
      [  let FitnessInfo info = st.fitnessInfo
             result = do
               wid <- st.selectedWorkout
               let sets = Array.concat (map (\(Id x) -> x.values) <$> info.setsForWeek)
               pure $ pastWorkoutSets st.timezoneOffset wid sets
        in HH.table_ $ maybe [] identity result
      ]

      where

      pastWorkoutSets timezoneOffset (WorkoutId wid) =
        map pastSet
          <<< Array.take 10
          <<< Array.groupBy (\(WorkoutSet a) (WorkoutSet b) -> sameDay st.timezoneOffset a.date b.date)
          <<< filter (\(WorkoutSet w) -> w.workout == wid)

        where

          pastSet :: NonEmptyArray WorkoutSet -> H.ComponentHTML Action Slots m
          pastSet xs =
            let (WorkoutSet ws) = NEArray.head xs
                date = displayDate timezoneOffset ws.date
            in setGroup date (NEArray.toArray xs)

    thisWeekSets =
      let (FitnessInfo info) = st.fitnessInfo
      in maybe [] identity (Array.head info.setsForWeek)

    lastWeekSets =
      let (FitnessInfo info) = st.fitnessInfo
      in maybe [] identity (Array.index info.setsForWeek 1)

    renderWorkoutVolume :: H.ComponentHTML Action Slots m
    renderWorkoutVolume =
      HH.div_ $
        map mkHtml $
        Array.filter (\x -> Array.any ((/=) 0) x.counts) $
        map mkCounts allKeys

      where

      mkHtml x = HH.p_ case Array.uncons x.counts of
          Just {head: c, tail: cs} ->
            [HH.text $ x.muscle <> ": " <> show c <> " ← " <> intercalate ", " (map show cs)]
          Nothing -> []

      (FitnessInfo info) = st.fitnessInfo

      mkCounts m =
        { muscle: m
        , counts: map
            (\vs -> maybe 0 identity (Map.lookup m vs))
            volumes
        }

      allKeys =
        Array.fromFoldable
          $ map (\(Id {values: Muscle m}) -> m.name)
          $ Map.values info.muscles

      volumes = map (foldr buildCount Map.empty) info.setsForWeek

      buildCount (Id {values: WorkoutSet w}) answer =
        let (FitnessInfo info) = st.fitnessInfo
            result = do
              muscleId <- Map.lookup (WorkoutId w.workout) info.primaryMuscles
              Id {values: Muscle m} <- Map.lookup muscleId info.muscles
              pure m.name
        in Map.alter (Just <<< maybe 1 ((+) 1)) (maybe "unknown" identity result) answer

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
      in HH.tr [ HH.class_ (ClassName $ intensityColor ws.intensity) ]
           (maybe [HH.th_ [HH.text "fail"]] identity result)

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
      verifySets <- getJson "sets?weeks=3"

      H.modify_ $ const $ either Error Full $ do

        -- Verify all the data from API is valid
        workouts <- verifyWorkouts
        muscles <- verifyMuscles
        primaryMusclePairs <- verifyPrimaryMuscles
        sets :: Array (Array (Id WorkoutSet)) <- verifySets

        -- Transform Values
        let primaryMuscles =
              Array.foldr
              (\(PrimaryMuscle {workout, muscle}) -> Map.insert (WorkoutId workout) (MuscleId muscle))
              Map.empty
              primaryMusclePairs

        pure $ Info
          { fitnessInfo: FitnessInfo
            { sets: Map.unions (mapFromIds WorkoutSetId <$> sets)
            , setsForWeek: sets
            , workouts: mapFromIds WorkoutId workouts
            , muscles: mapFromIds MuscleId muscles
            , primaryMuscles: primaryMuscles
            }
          , selectedWorkout: Nothing
          , today: maybe today identity $ DateTime.adjust (Minutes $ negate offset) today
          , timezoneOffset: Minutes $ negate offset
          }
    Submit -> do
      H.get >>= case _ of
        Empty -> pure unit
        Error e -> pure unit
        Full (Info st) -> do
          let gather = runMaybeT $ do
                WorkoutId workoutId <- MaybeT $ pure st.selectedWorkout
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
              postJson ws "/sets" >>= case _ of
                Right (Id newSet) -> H.modify_ $ case _ of
                    Full (Info ss) ->
                      let FitnessInfo info = ss.fitnessInfo
                      in Full $ Info ss
                           { fitnessInfo = FitnessInfo $ info
                             { sets = Map.insert (WorkoutSetId newSet.id) (Id newSet) info.sets
                             , setsForWeek
                                 = maybe info.setsForWeek identity
                                 $ Array.modifyAt 0 (Array.cons (Id newSet)) info.setsForWeek
                             }
                           }
                    prev -> prev
                Left _ -> pure unit
            Nothing -> pure unit
    WorkoutSelected (WorkoutSelector.Selection w) -> do
      H.modify_ $ \st -> case st of
        Full (Info i) -> Full $ Info i
          { selectedWorkout = Just w
          }
        _ -> st

recommendedWeights :: forall m. Info -> H.ComponentHTML Action Slots m
recommendedWeights (Info {selectedWorkout, fitnessInfo: FitnessInfo info}) = HH.text $
  let result = do
        workoutId <- selectedWorkout
        Id w <- Map.lookup workoutId info.workouts
        let matchWorkout = (\(Id {values: WorkoutSet s}) -> s.workout == w.id)
        Id {values: WorkoutSet x} <- head $ filter matchWorkout $ Array.fromFoldable $ Map.values info.sets
        pure $ "Weight for Previous Set: " <> show x.weight
  in maybe "" identity result

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


setGroup :: forall m. String -> Array WorkoutSet -> H.ComponentHTML Action Slots m
setGroup setTxt sets =
  let toBlock (WorkoutSet ws) =
        HH.th [ HH.class_ (ClassName $ intensityColor ws.intensity)]
          [ HH.text $ show ws.weight <> "x" <> show ws.reps ]
      cols = Array.cons (HH.th_ [ HH.text setTxt ]) (toBlock <$> Array.reverse sets)
  in HH.tr_ cols

sameDay :: Minutes -> Number -> Number -> Boolean
sameDay offset x y =
  let result = do
        xDateTime <- unixEpochToDateTime offset x
        yDateTime <- unixEpochToDateTime offset y
        pure $ day (date xDateTime) == day (date yDateTime)
  in maybe false identity result

intensityColor :: Int -> String
intensityColor 4 = "failSet"
intensityColor 3 = "hardSet"
intensityColor 2 = "goodSet"
intensityColor 1 = "easySet"
intensityColor _ = "noEffortSet"
