module Main where

import Prelude (Unit, bind, const, pure, show, unit, ($), (<>), (<$>))

import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Data.Argonaut (class DecodeJson, decodeJson, jsonParser, (.:))
import Data.Either (Either(..), either)
import Data.Formatter.DateTime (FormatterCommand(..), format, unformat)
import Data.HTTP.Method (Method(..))
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (div, text, table_, tr_, th_, li_, ul_) as HH
import Halogen.HTML.Properties (class_) as HH
import Halogen.VDom.Driver (runUI)
import Web.HTML.Common (ClassName(..))

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

data State
  = Empty
  | Full Info
  | Error String

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval {
      handleAction = handleAction,
      initialize = Just Init
      }
    }
  where
  initialState :: forall a. a -> State 
  initialState _ = Empty 

  render :: State -> H.ComponentHTML Action () m
  render Empty = HH.text "No data yet"
  render (Full (Info state)) = HH.div [ HH.class_ (ClassName "content") ]
    [ HH.table_ (renderSet <$> state.sets)
    , HH.ul_ (renderWorkout <$> state.workouts)
    ]

    where

    renderWorkout w = HH.li_ [ HH.text w ]

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

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    Init -> do
      -- response <- H.liftAff $ AX.request $ AX.defaultRequest
      --   { url = "/sets"
      --   , method = Left GET
      --   , responseFormat = AXRF.string
      --   }
      -- let result = case response of
      --       Right r -> case jsonParser r.body of
      --         Right json -> case decodeJson json of
      --           Right sets -> Full (Info {sets: sets, workouts: []})
      --           Left _ -> Error "Problem decoding json" 
      --         Left _ -> Error "Problem parsing json"
      --       Left _ -> Error "Problem making request" 
      -- H.modify_ $ const result 
      verifySets <- getJson "/sets"
      verifyWorkouts <- getJson "workouts"
      H.modify_ $ const $ either Error Full $ do
        sets <- verifySets
        workouts <- verifyWorkouts
        pure $ Info {sets: sets, workouts: workouts}

  getJson :: forall a. DecodeJson a => String -> H.HalogenM State Action () output m (Either String a)
  getJson endpoint = do 
      response <- H.liftAff $ AX.request $ AX.defaultRequest
        { url = endpoint 
        , method = Left GET
        , responseFormat = AXRF.string
        }
      pure $ case response of
        Right r -> case jsonParser r.body of
          Right json -> case decodeJson json of
            Right x -> Right x
            Left _ -> Left "Problem decoding json" 
          Left _ -> Left "Problem parsing json"
        Left _ -> Left "Problem making request" 

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
