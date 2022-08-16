module Time (Time, getCurrentTime, sameDay, displayDate) where

import Data.Maybe
import Prelude
import Halogen as H
import Effect.Now (now, nowDateTime)
import Data.DateTime (DateTime, date, day)
import Data.DateTime (adjust) as DateTime
import Data.DateTime.Instant (instant, unInstant) as Date
import Data.Time.Duration (Minutes(..))
import Data.Time.Duration as Time
import Data.Formatter.DateTime
  (format, FormatterCommand(DayOfMonthTwoDigits, Placeholder,
                            MonthTwoDigits, YearFull))
import Data.JSDate as JSDate
import Data.List as List
import Data.List (List)
import Data.Argonaut (class DecodeJson, class EncodeJson, jsonEmptyObject, decodeJson, (~>), (:=), (.:))

newtype Time = Time Number

derive newtype instance timeDecodeJson :: DecodeJson Time
derive newtype instance timeEncodeJson :: EncodeJson Time

getCurrentTime = do
  Time.Milliseconds date <- Date.unInstant <$> H.liftEffect now
  pure $ Time $ date / 1000.0

sameDay :: Minutes -> Time -> Time -> Boolean
sameDay offset (Time x) (Time y) =
  let result = do
        xDateTime <- unixEpochToDateTime offset x
        yDateTime <- unixEpochToDateTime offset y
        pure $ day (date xDateTime) == day (date yDateTime)
  in maybe false identity result

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

displayDate :: Minutes -> Time -> String
displayDate offset (Time date) = case unixEpochToDateTime offset date of
  Just dt -> format dateWriteFormat dt
  Nothing -> "Failed to parse date"
