{-# LANGUAGE CPP #-}

-- |
-- Module: Pact.Time.Format
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Pact.Time.Format
(
-- * Formatting and Parsing
  parseTime
, formatTime
) where

#if WITH_TIME
import Pact.Time.Format.External
#else
import Pact.Time.Format.Internal
#endif

