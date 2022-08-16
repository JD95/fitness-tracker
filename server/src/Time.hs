{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Time (Time, Weeks(..), previousSundays, Time.getCurrentTime, sameDay) where

import Data.Aeson
import Data.Fixed
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Data.Time.Calendar (Day, addDays, dayOfWeek, DayOfWeek(Sunday))
import Data.Time.Clock (UTCTime (..), nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import qualified Data.Time.Clock as UTC
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)

newtype Time = Time Double
  deriving stock (Show, Eq, Ord)
  deriving newtype (ToJSON, FromJSON, FromField, ToField)

newtype Weeks = Weeks Int
  deriving (Eq, Ord, Show)

daysSinceSunday :: Day -> Int
daysSinceSunday = fromEnum . dayOfWeek

utcTimeToDouble :: UTCTime -> Double
utcTimeToDouble t =
  let (MkFixed nWeeksAgoPico) = nominalDiffTimeToSeconds $ utcTimeToPOSIXSeconds t
   in (fromIntegral nWeeksAgoPico :: Double) / (10.0 ^ 12)


doubleToUtcTime ::  Double -> UTCTime
doubleToUtcTime =
  posixSecondsToUTCTime
    . secondsToNominalDiffTime
    . fromIntegral
    . floor

previousSunday :: UTCTime -> UTCTime
previousSunday today =
  let diff = case dayOfWeek (utctDay today) of
        Sunday -> case utctDayTime today of
          0 -> 7
          _ -> 0
        _ -> daysSinceSunday (utctDay today)
   in UTCTime (addDays (fromIntegral $ negate diff) (utctDay today)) 0

previousSundays :: Time -> [Time]
previousSundays (Time today) = Time . utcTimeToDouble <$> iterate previousSunday (doubleToUtcTime today)

getCurrentTime :: IO Time
getCurrentTime = Time . utcTimeToDouble <$> UTC.getCurrentTime

sameDay :: Time -> Time -> Bool
sameDay (Time x) (Time y) = utctDay (doubleToUtcTime x) == utctDay (doubleToUtcTime y)
