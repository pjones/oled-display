-- |
--
-- Copyright:
--   This file is part of the package oled-display. It is subject to the
--   license terms in the LICENSE file found in the top-level directory
--   of this distribution and at:
--
--     https://code.devalot.com/pjones/oled-display.git
--
--   No part of this package, including this file, may be copied,
--   modified, propagated, or distributed except according to the terms
--   contained in the LICENSE file.
--
-- License: BSD-2-Clause
module Display.Message
  ( Message (..),
  )
where

import Display.Timer (Timer)

data Message
  = -- | Start a new timer.
    TimerStart Timer
  | -- | The timer stopped.
    TimerStop
  | -- | The message associated with the current timer changed.
    TimerMessage ByteString
  deriving (Show)
