{-# LANGUAGE CPP #-}

-- |
-- Module: Data.Time.System
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Returns the time of the system clock as 64-bit value that counts microseconds
-- since the POSIX epoch.
--
module Data.Time.System
( getSystemTimeMicros
) where

import Data.Int (Int64)

#if WITH_TIME
import qualified Data.Time.Clock.POSIX (getPOSIXTime)
#else
import System.Clock (getTime, TimeSpec(..), Clock(Realtime))
#endif

getSystemTimeMicros :: IO Int64
getSystemTimeMicros = do

#if WITH_TIME
    s <- Data.Time.Clock.POSIX.getPOSIXTime
    return $ round $ s * 1000000
#else
    TimeSpec s ns <- getTime Realtime
    return $ (s * 1000000) + (ns `quot` 1000)
#endif

{-# INLINE getSystemTimeMicros #-}

