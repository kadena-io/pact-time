{-# LANGUAGE CPP #-}

-- |
-- Module: Data.Time.Format
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Data.Time.Format
(
-- * Formatting and Parsing
  parseTime
, formatTime
) where

#if WITH_TIME
import Data.Time.Format.External
#else
import Data.Time.Format.Internal
#endif

