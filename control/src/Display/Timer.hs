{-# LANGUAGE DeriveFunctor #-}

{-|

Copyright:
  This file is part of the package oled-display. It is subject to the
  license terms in the LICENSE file found in the top-level directory
  of this distribution and at:

    https://code.devalot.com/pjones/oled-display.git

  No part of this package, including this file, may be copied,
  modified, propagated, or distributed except according to the terms
  contained in the LICENSE file.

License: BSD-2-Clause

-}
module Display.Timer
  ( Timer
  , Remaining(..)
  , pomodoro
  , remaining
  , render
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.Time.Calendar (Day(..))
import Data.Time.Clock (UTCTime(..), NominalDiffTime, diffUTCTime, addUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

--------------------------------------------------------------------------------
-- | Representation of a timer.
data Timer
  = Pomodoro UTCTime
    -- ^ Pomodoro timer started at the given point in time.

  deriving Show

--------------------------------------------------------------------------------
-- | Some remaining interval of time.
data Remaining a
  = Negative a
    -- ^ Timer expired and is now growing in the negative direction
    -- but @a@ is expressed as a positive value.

  | Positive a
    -- ^ Timer is still counting down with @a@ representing the amount
    -- of time left.

  deriving (Show, Functor)

--------------------------------------------------------------------------------
extractR :: Remaining a -> a
extractR (Negative t) = t
extractR (Positive t) = t

--------------------------------------------------------------------------------
-- | Number of seconds in a pomodoro.
pomodoroSec :: NominalDiffTime
pomodoroSec = 1500

--------------------------------------------------------------------------------
-- | Start a pomodoro given the number of seconds since the Unix epoch.
pomodoro :: (Real a) => a -> Timer
pomodoro = Pomodoro . posixSecondsToUTCTime . realToFrac

--------------------------------------------------------------------------------
-- | Return the number of remaining seconds in the timer.
remaining :: Timer -> UTCTime -> Remaining NominalDiffTime
remaining timer now =
  case timer of
    Pomodoro t -> pomo t

  where
    diff = diffUTCTime now
    pomo start = if diff start > pomodoroSec
                   then Negative (diff start - pomodoroSec)
                   else Positive (pomodoroSec - diff start)

--------------------------------------------------------------------------------
-- | Render the timer as a string.
render :: Timer -> UTCTime -> ByteString
render timer now =
  ByteString.pack $
    formatTime defaultTimeLocale
               (format asUTCTime)
               (extractR asUTCTime)
  where
    format :: Remaining UTCTime -> String
    format (Negative _) = "-%M:%S"
    format (Positive _) = "%M:%S"

    asUTCTime :: Remaining UTCTime
    asUTCTime = toUTCTime <$> remaining timer now

    -- Only needed for older version of the time package:
    toUTCTime :: NominalDiffTime -> UTCTime
    toUTCTime t = addUTCTime t (UTCTime (ModifiedJulianDay 0) (fromIntegral (0::Int)))
