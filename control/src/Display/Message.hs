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
module Display.Message
  ( Message(..)
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Data.ByteString.Char8 (ByteString)

--------------------------------------------------------------------------------
-- Project Imports:
import Display.Timer (Timer)

--------------------------------------------------------------------------------
data Message
  = TimerStart Timer
    -- ^ Start a new timer.

  | TimerStop
    -- ^ The timer stopped.

  | TimerMessage ByteString
    -- ^ The message associated with the current timer changed.

  deriving Show
