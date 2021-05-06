{-# LANGUAGE CPP #-}

-- |
-- Module: Data.Time
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- A minimal time library for usage with the [Pact Smart Contract
-- Language](https://github.com/kadena-io/pact/).
--
-- The focus of this library is on minimality, performance, and binary level
-- stability. Time is represented as 64-bit integral value that counts nominal
-- micro-seconds since the modified Julian date epoch (MJD). The implementation
-- ignores leap seconds.
--
-- While the library can parse date-time values with time zones, internally all
-- date-times are represented as UTC and formatting only supports UTC. Only
-- the default English language locale is supported.
--
-- Detail about supported formats can be found in the [Pact Language
-- Reference](https://pact-language.readthedocs.io/en/stable/pact-reference.html#time-formats).
--
module Data.Time
(
-- * NominalDiffTime
  NominalDiffTime(..)
, toMicroseconds
, fromMicroseconds
, toSeconds
, fromSeconds
, nominalDay

-- * UTCTime
, UTCTime
, getCurrentTime
, day
, dayTime
, fromDayAndDayTime
, toPosixTimestampMicros
, fromPosixTimestampMicros
, posixEpoch
, mjdEpoch

-- * Formatting and Parsing
, parseTime
, formatTime

-- * Reexports
, AffineSpace(..)
, VectorSpace(..)
) where

import Data.Time.Format
import Data.Time.Internal

