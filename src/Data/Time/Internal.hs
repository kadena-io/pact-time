{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Data.Time.Internal
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- This is an internal module. No guarantee is provided regarding the stability
-- of the functions in this module. Use at your own risk.
--
module Data.Time.Internal
(
  Micros
, Day

-- * NominalDiffTime
, NominalDiffTime(..)
, toMicroseconds
, fromMicroseconds
, toSeconds
, fromSeconds
, nominalDay

-- * UTCTime
, UTCTime(..)
, getCurrentTime
, day
, dayTime
, fromDayAndDayTime
, toPosixTimestampMicros
, fromPosixTimestampMicros
, mjdEpoch
, posixEpoch

-- * Julian Dates
, ModifiedJulianDay(..)
, ModifiedJulianDate(..)
, toModifiedJulianDate
, fromModifiedJulianDate

-- * Reexports
, AffineSpace(..)
, VectorSpace(..)
) where

import Control.DeepSeq

import Data.AdditiveGroup
import Data.AffineSpace
import Data.Decimal
import Data.Serialize
import Data.VectorSpace

import GHC.Generics hiding (from)
import GHC.Int (Int64)

import Lens.Micro

-- internal modules

import Data.Time.System

-- -------------------------------------------------------------------------- --
-- Types for internal representations

type Micros = Int64
type Day = Int

-- -------------------------------------------------------------------------- --
-- Nominal Diff Time

-- | A time interval as measured by UTC, that does not take leap-seconds into
-- account.
--
newtype NominalDiffTime = NominalDiffTime { _microseconds :: Micros }
    deriving (Eq, Ord)
    deriving newtype (NFData)

-- | Convert from 'NominalDiffTime' to a 64-bit representation of microseconds.
--
toMicroseconds :: NominalDiffTime -> Micros
toMicroseconds = _microseconds
{-# INLINE toMicroseconds #-}

-- | Convert from a 64-bit representation of microseconds to 'NominalDiffTime'.
--
fromMicroseconds :: Micros -> NominalDiffTime
fromMicroseconds = NominalDiffTime
{-# INLINE fromMicroseconds #-}

instance AdditiveGroup NominalDiffTime where
    zeroV = NominalDiffTime 0
    NominalDiffTime a ^+^ NominalDiffTime b = NominalDiffTime (a + b)
    negateV (NominalDiffTime v) = NominalDiffTime (-v)
    NominalDiffTime a ^-^ NominalDiffTime b = NominalDiffTime (a - b)
    {-# INLINE zeroV #-}
    {-# INLINE (^+^) #-}
    {-# INLINE negateV #-}
    {-# INLINE (^-^) #-}

instance VectorSpace NominalDiffTime where
    type Scalar NominalDiffTime = Rational
    s *^ (NominalDiffTime m) = NominalDiffTime $ round (s * fromIntegral m)
    {-# INLINE (*^) #-}

-- | Serializes 'NominalDiffTime' as 64-bit signed microseconds in little endian
-- encoding.
--
instance Serialize NominalDiffTime where
    put (NominalDiffTime m) = putInt64le m
    get = NominalDiffTime <$> getInt64le
    {-# INLINE put #-}
    {-# INLINE get #-}

-- | Convert from 'NominalDiffTime' to a 'Decimal' representation of seconds.
--
toSeconds :: NominalDiffTime -> Decimal
toSeconds (NominalDiffTime m) = realToFrac m / 1000000
{-# INLINE toSeconds #-}

-- | Convert from 'Decimal' representation of seconds to 'NominalDiffTime'.
--
-- The result is rounded using banker's method, i.e. remainders of 0.5 a rounded
-- to the next even integer.
--
fromSeconds :: Decimal -> NominalDiffTime
fromSeconds d = NominalDiffTime $ round $ d * 1000000
{-# INLINE fromSeconds #-}

-- | The nominal length of a day: precisely 86400 SI seconds.
--
nominalDay :: NominalDiffTime
nominalDay = NominalDiffTime $ 86400 * 1000000
{-# INLINE nominalDay #-}

toPosixTimestampMicros :: UTCTime -> Micros
toPosixTimestampMicros = toTimestampMicros . toPosix
{-# INLINE toPosixTimestampMicros #-}

fromPosixTimestampMicros :: Micros -> UTCTime
fromPosixTimestampMicros = fromPosix . fromTimestampMicros
{-# INLINE fromPosixTimestampMicros #-}

-- -------------------------------------------------------------------------- --
-- UTCTime

-- | UTCTime with microseconds precision. Internally it is represented as 64-bit
-- count nominal microseconds since MJD Epoch.
--
-- This implementation ignores leap seconds. Time differences are  measured as
-- nominal time, with a nominal day having exaxtly @24 * 60 * 60@ SI seconds. As
-- a consequence the difference between two dates as computed by this module is
-- generally equal or smaller than what is actually measured by a clock.
--
newtype UTCTime = UTCTime { _utcTime :: NominalDiffTime }
    deriving (Eq, Ord)
    deriving (Generic)
    deriving newtype (NFData)
    deriving newtype (Serialize)

instance AffineSpace UTCTime where
    type Diff UTCTime = NominalDiffTime
    UTCTime a .-. UTCTime b = a ^-^ b
    UTCTime a .+^ b = UTCTime (a ^+^ b)
    {-# INLINE (.-.) #-}
    {-# INLINE (.+^) #-}

getCurrentTime :: IO UTCTime
getCurrentTime = UTCTime . (^+^ _utcTime posixEpoch) . _posixTime
    <$> getPOSIXTime
{-# INLINE getCurrentTime #-}

-- | The date of a UTCTime value represented as modified Julian 'Day'.
--
day :: Lens' UTCTime ModifiedJulianDay
day = lens
    (_mjdDay . toModifiedJulianDate)
    (\a b -> fromModifiedJulianDate . set mjdDay b $ toModifiedJulianDate a)
{-# INLINE day #-}

-- | The day time of a 'UTCTime' value represented as 'NominalDiffTime' since
-- @00:00:00@ of that respective day.
--
dayTime :: Lens' UTCTime NominalDiffTime
dayTime = lens
    (_mjdTime . toModifiedJulianDate)
    (\a b -> fromModifiedJulianDate . set mjdTime b $ toModifiedJulianDate a)
{-# INLINE dayTime #-}

-- | Create a 'UTCTime' from a date and a daytime. The date is represented
-- as modified Julian 'Day' and the day time is represented as
-- 'NominalDiffTime' since '00:00:00' of the respective day.
--
-- Note that this implementation does not support representation of leap
-- seconds.
--
fromDayAndDayTime :: ModifiedJulianDay -> NominalDiffTime -> UTCTime
fromDayAndDayTime d t = fromModifiedJulianDate $ ModifiedJulianDate d t
{-# INLINE fromDayAndDayTime #-}

-- | The POSIX Epoch represented as UTCTime.
--
posixEpoch :: UTCTime
posixEpoch = UTCTime (fromIntegral d *^ nominalDay)
  where
    ModifiedJulianDay d = posixEpochDay
{-# INLINE posixEpoch #-}

-- | The Epoch of the modified Julian day represented as 'UTCTime'.
--
mjdEpoch :: UTCTime
mjdEpoch = UTCTime zeroV
{-# INLINE mjdEpoch #-}

-- -------------------------------------------------------------------------- --
-- POSIX Timestamps

-- | POSIX time is the nominal time since 1970-01-01 00:00 UTC. It is
-- represented as 64-bit count of microseconds.
--
-- Users who only need POSIX timestamps can ignore this type and just use
-- 'UTCTime' with 'toPosxiTimestampMicros' and 'fromPosixTimestampMicros'.
--
newtype POSIXTime = POSIXTime { _posixTime :: NominalDiffTime }
    deriving (Eq, Ord)
    deriving newtype (NFData)

-- | Represent POSIXTime as 64-bit value of microseconds since 'posixEpoch'.
--
toTimestampMicros :: POSIXTime -> Micros
toTimestampMicros = _microseconds . _posixTime
{-# INLINE toTimestampMicros #-}

-- | Create POSIXTime from 64-bit value of microseconds since 'posixEpoch'.
--
fromTimestampMicros :: Micros -> POSIXTime
fromTimestampMicros = POSIXTime . fromMicroseconds
{-# INLINE fromTimestampMicros #-}

-- | The day of the epoch of 'SystemTime', 1970-01-01
--
posixEpochDay :: ModifiedJulianDay
posixEpochDay = ModifiedJulianDay 40587
{-# INLINE posixEpochDay #-}

-- | Get current POSIX time
--
getPOSIXTime :: IO POSIXTime
getPOSIXTime = POSIXTime . NominalDiffTime . fromIntegral <$> getSystemTimeMicros
{-# INLINE getPOSIXTime #-}

-- The following conversions between POSIXTime and UTCTime are efficient because
-- all constants are inlined.

-- | Convert from UTCTime to POSIXTime
--
toPosix :: UTCTime -> POSIXTime
toPosix t = POSIXTime $ _utcTime t ^-^ _utcTime posixEpoch
{-# INLINE toPosix #-}

-- | Convert from POSIXTime to UTCTime
--
fromPosix :: POSIXTime -> UTCTime
fromPosix p = UTCTime $ _posixTime p ^+^ _utcTime posixEpoch
{-# INLINE fromPosix #-}

-- -------------------------------------------------------------------------- --
-- Modified Julian Day Representation of UTC

newtype ModifiedJulianDay = ModifiedJulianDay Day
    deriving newtype (Eq, Ord, NFData)

-- | Modified Julian Day Representation of UTC
--
data ModifiedJulianDate = ModifiedJulianDate
    { _mjdDay :: !ModifiedJulianDay
    , _mjdTime :: !NominalDiffTime
    }
    deriving (Eq, Ord, Generic)
    deriving anyclass (NFData)

mjdDay :: Lens' ModifiedJulianDate ModifiedJulianDay
mjdDay = lens _mjdDay $ \a b -> a { _mjdDay = b }
{-# INLINE mjdDay #-}

mjdTime :: Lens' ModifiedJulianDate NominalDiffTime
mjdTime = lens _mjdTime $ \a b -> a { _mjdTime = b }
{-# INLINE mjdTime #-}

-- | Convert from 'UTCTime' to modified 'Julian' Day time.
--
toModifiedJulianDate :: UTCTime -> ModifiedJulianDate
toModifiedJulianDate (UTCTime (NominalDiffTime m)) = ModifiedJulianDate
    (ModifiedJulianDay (fromIntegral d))
    (NominalDiffTime t)
  where
    (d, t) = divMod m n
    NominalDiffTime n = nominalDay
{-# INLINE toModifiedJulianDate #-}

-- | Convert from modified 'Julian' Day time to 'UTCTime'.
--
fromModifiedJulianDate :: ModifiedJulianDate -> UTCTime
fromModifiedJulianDate (ModifiedJulianDate (ModifiedJulianDay d) t)
    = UTCTime $ (fromIntegral d *^ nominalDay) ^+^ t
{-# INLINE fromModifiedJulianDate #-}

