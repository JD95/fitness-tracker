module Main where

import Prelude
  (Unit, Void, bind, const, discard,
   identity, join, map, negate,
   pure, show, unit, ($), (+),
   (<$>), (<>), (=<<), (/=),
   (==), (>>=), (<<<))

import Data.Eq (class Eq)
import Data.Traversable (foldr, intercalate, traverse_)
import Data.Newtype (un)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Except.Trans
import Control.Monad.Trans.Class (lift)
import Data.Array (filter, head)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEArray
import Data.Array as Array
import Data.DateTime (DateTime)
import Data.DateTime (adjust) as DateTime
import Data.Either (either)
import Data.JSDate as JSDate
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Natural (Natural, natToInt)
import Data.Time.Duration (Minutes(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Now (nowDateTime)
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
import Time (displayDate, getCurrentTime, sameDay) as Time

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
    , selectedWorkoutSets :: Maybe (Array (NonEmptyArray (Id WorkoutSet)))
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

weightSlot :: Int
weightSlot = 0

repsSlot :: Int
repsSlot = 1

intensitySlot :: Int
intensitySlot = 2

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
  render (Error e) = HH.text e
  render (Full state@(Info st)) = HH.div [ HH.class_ (ClassName "content") ]
    [ HH.div [ HH.class_ (ClassName "col") ]
      [ renderWorkoutVolume (Info st)
      , renderSetsGrouped (Info st) thisWeekSets
      ]
    , HH.div [ HH.class_ (ClassName "col") ]
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
      , workoutSetHistory (Info st)
      ]
    ]

    where

    thisWeekSets =
      let (FitnessInfo info) = st.fitnessInfo
      in maybe [] identity (Array.head info.setsForWeek)

    lastWeekSets =
      let (FitnessInfo info) = st.fitnessInfo
      in maybe [] identity (Array.index info.setsForWeek 1)

renderSetsGrouped :: forall m. Info -> Array (Id WorkoutSet) -> H.ComponentHTML Action Slots m
renderSetsGrouped (Info st) =
  HH.table [ HH.class_ (ClassName "pastWorkoutTable") ]
    <<< map mkSetGroup
    <<< Array.concat
    <<< map (Array.groupBy (eqOn (_.workout <<< un WorkoutSet)))
    <<< map (NEArray.toArray <<< map (_.values <<< un Id))
    <<< Array.groupBy (workoutSetSameDay st.timezoneOffset)

  where

  FitnessInfo info = st.fitnessInfo

  nameLookup x = do
    Id {values: Workout w} <- Map.lookup x info.workouts
    pure w.name

  mkSetGroup sets = setGroup name (NEArray.toArray sets) where
    WorkoutSet ws = NEArray.head sets
    name = maybe "Unknown" identity (nameLookup (WorkoutId ws.workout))

workoutSetHistory :: forall m. Info -> H.ComponentHTML Action Slots m
workoutSetHistory (Info st) = HH.div_ $
  [  HH.table_ $
       maybe [] identity $
       (map <<< map) (pastSet st.timezoneOffset) $
       st.selectedWorkoutSets
  ]

  where

  FitnessInfo info = st.fitnessInfo

  pastSet timezoneOffset sets =
    let xs = map (_.values <<< un Id) sets
        (WorkoutSet ws) = NEArray.head xs
        date = Time.displayDate timezoneOffset ws.date
    in setGroup date (NEArray.toArray xs)

renderWorkoutVolume :: forall m. Info -> H.ComponentHTML Action Slots m
renderWorkoutVolume (Info st) =
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

renderSet :: forall action slots m. Info -> Id WorkoutSet -> H.ComponentHTML action slots m
renderSet (Info st) (Id {id, values: WorkoutSet ws}) =
  let FitnessInfo info = st.fitnessInfo
      result = do
        Id {values: Workout w} <- Map.lookup (WorkoutId ws.workout) info.workouts
        pure $
          [ HH.th_ [ HH.text (Time.displayDate st.timezoneOffset ws.date) ]
          , HH.th_ [ HH.text w.name ]
          , HH.th_ [ HH.text $ show ws.reps ]
          , HH.th_ [ HH.text $ show ws.weight ]
          , HH.th_ [ HH.text $ show ws.intensity ]
          ]
  in HH.tr [ HH.class_ (ClassName $ intensityColor ws.intensity) ]
       (maybe [HH.th_ [HH.text "fail"]] identity result)

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action Slots output m Unit
handleAction = case _ of
  Init -> actionInit
  Submit -> actionSubmit
  WorkoutSelected (WorkoutSelector.Selection w) -> do
    H.modify_ $ updateFull $ \(Info i) -> Info i { selectedWorkout = Just w }
    fillWorkoutSets w

actionSubmit :: forall output m. MonadAff m => H.HalogenM State Action Slots output m Unit
actionSubmit = H.get >>= case _ of
  Empty -> pure unit
  Error e -> pure unit
  Full (Info st) -> do
    gatherFieldData (Info st) >>= traverse_ \ws ->
      postJson ws "/sets" >>= traverse_ \(Id newSet) -> do
        H.modify_ $ updateFull $ \(Info info) ->
          let (FitnessInfo fitInfo) = info.fitnessInfo
              sets' =
                Map.insert (WorkoutSetId newSet.id) (Id newSet) fitInfo.sets
              setsForWeek' =
                fromMaybe fitInfo.setsForWeek $ Array.modifyAt 0 (Array.cons (Id newSet)) fitInfo.setsForWeek
              selectedWorkoutSets' =
                addSetToTable st.timezoneOffset (Id newSet) <$> info.selectedWorkoutSets
          in Info info
             { fitnessInfo = FitnessInfo fitInfo
                 { sets = sets', setsForWeek = setsForWeek' }
             , selectedWorkoutSets = selectedWorkoutSets'
             }

gatherFieldData :: forall output m. MonadAff m => Info -> H.HalogenM State Action Slots output m (Maybe WorkoutSet)
gatherFieldData (Info st) = runMaybeT $ do
  WorkoutId workoutId <- MaybeT $ pure st.selectedWorkout
  -- Multiple input fields, so we need slots for them
  weight <- MaybeT $ join <$> H.request InputField.natProxy weightSlot InputField.GetValue
  reps <- MaybeT $ join <$> H.request InputField.natProxy repsSlot InputField.GetValue
  intensity <- MaybeT $ H.request RadioInput.proxy intensitySlot RadioInput.GetValue
  time <- lift $ Time.getCurrentTime
  pure $ WorkoutSet
    { workout: workoutId
    , reps: natToInt reps
    , date: time
    , weight: natToInt weight
    , intensity: intensity
    }

actionInit :: forall output m. MonadAff m => H.HalogenM State Action Slots output m Unit
actionInit = do
  -- Get Time Info
  today <- H.liftEffect nowDateTime
  offset <- H.liftEffect $ JSDate.getTimezoneOffset =<< JSDate.now

  H.modify_ <<< const <<< either Error Full =<< runExceptT do

    -- Make API calls for data
    workouts <- ExceptT $ getJson "workouts"
    primaryMusclePairs <- ExceptT $ getJson "primary-muscles"
    muscles <- ExceptT $ getJson "muscles"
    sets :: Array (Array (Id WorkoutSet)) <- ExceptT $ getJson "sets?weeks=3"

    -- Transform Values
    let primaryMuscles =
          Array.foldr
          (\(PrimaryMuscle {workout, muscle}) ->
            Map.insert (WorkoutId workout) (MuscleId muscle))
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
      , selectedWorkoutSets: Nothing
      , today: maybe today identity $ DateTime.adjust (Minutes $ negate offset) today
      , timezoneOffset: Minutes $ negate offset
      }

addSetToTable :: Minutes -> Id WorkoutSet -> Array (NonEmptyArray (Id WorkoutSet)) -> Array (NonEmptyArray (Id WorkoutSet))
addSetToTable offset newSet arr =
  let newRow = Array.cons (NEArray.singleton newSet) arr
  in case Array.index arr 0 of
    Just ws ->
      if workoutSetSameDay offset newSet (NEArray.head ws)
      then fromMaybe newRow (Array.modifyAt 0 (NEArray.cons newSet) arr)
      else newRow
    Nothing -> newRow

fillWorkoutSets :: forall a b c m. MonadAff m => WorkoutId -> H.HalogenM State a b c m Unit
fillWorkoutSets (WorkoutId wid) = do
  getJson ("workout" <> "/" <> show wid <> "/sets") >>= traverse_
    \sets -> H.modify_ $ updateFull $ \(Info i) ->
      Info i { selectedWorkoutSets = Just (sets :: Array (NonEmptyArray (Id WorkoutSet))) }

updateFull :: (Info -> Info) -> State -> State
updateFull f (Full info) = Full (f info)
updateFull _ prev = prev

updateFitnessInfo :: (FitnessInfo -> FitnessInfo) -> Info -> Info
updateFitnessInfo f (Info info) = Info info { fitnessInfo = f info.fitnessInfo }

recommendedWeights :: forall m. Info -> H.ComponentHTML Action Slots m
recommendedWeights (Info {selectedWorkout, fitnessInfo: FitnessInfo info}) = HH.text $
  maybe "" identity $ do
    workoutId <- selectedWorkout
    Id w <- Map.lookup workoutId info.workouts
    let matchWorkout = (\(Id {values: WorkoutSet s}) -> s.workout == w.id)
    Id {values: WorkoutSet x} <-
      head $ filter matchWorkout $
      Array.fromFoldable $
      Map.values info.sets
    pure $ "Weight for Previous Set: " <> show x.weight

recommendedRepRange :: forall m. Info -> H.ComponentHTML Action Slots m
recommendedRepRange (Info {selectedWorkout, fitnessInfo: FitnessInfo info}) = HH.text $
  maybe "" identity $ do
    workoutId <- selectedWorkout
    muscleId <- Map.lookup workoutId info.primaryMuscles
    Id {values: Muscle m} <- Map.lookup muscleId info.muscles
    pure $ "Recommended Rep Range: (" <> show m.repsMin <> "-" <> show m.repsMax <> ")"

setGroup :: forall m. String -> Array WorkoutSet -> H.ComponentHTML Action Slots m
setGroup setTxt sets =
  let toBlock (WorkoutSet ws) =
        HH.th [ HH.class_ (ClassName $ intensityColor ws.intensity)]
          [ HH.text $ show ws.weight <> "x" <> show ws.reps ]
      cols = Array.cons (HH.th_ [ HH.text setTxt ]) (toBlock <$> Array.reverse sets)
  in HH.tr_ cols


workoutSetSameDay :: Minutes -> Id WorkoutSet -> Id WorkoutSet -> Boolean
workoutSetSameDay offset (Id x) (Id y) =
  Time.sameDay offset ((un WorkoutSet x.values).date) ((un WorkoutSet y.values).date)

intensityColor :: Int -> String
intensityColor 4 = "failSet"
intensityColor 3 = "hardSet"
intensityColor 2 = "goodSet"
intensityColor 1 = "easySet"
intensityColor _ = "noEffortSet"

eqOn :: forall a b. Eq b => (a -> b) -> (a -> a -> Boolean)
eqOn f x y = f x == f y
