{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Main
( main
) where

import Test.Tasty

-- internal modules

import qualified Test.Data.Time.Format (tests)

main :: IO ()
main = defaultMain $ testGroup "tests"
    [ Test.Data.Time.Format.tests
    ]
