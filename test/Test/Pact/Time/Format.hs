{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Test.Pact.Time.Format
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Test.Pact.Time.Format
( tests
, formats
) where

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Pact.Time

-- -------------------------------------------------------------------------- --
-- TODO: Test instances:

-- This may be different when the time package is used:
-- f = "%Y-%m-%dT%H:%M:%S%v%Z"
-- formatTime_ f t -> OK
-- parseTime_ f (formatTime_ f t) -> error

-- f = "%Y-%m-%dT%H:%M:%S.%v%Z"
-- formatTime_ f t -> OK
-- parseTime_ f (formatTime_ f t) -> OK

-- f = "%Y-%m-%dT%H:%M:%S%Q%Z"
-- formatTime_ f t -> OK
-- parseTime_ f (formatTime_ f t) -> OK

-- -------------------------------------------------------------------------- --
-- Tests

tests :: TestTree
tests = testGroup "Test.Pact.Time.Format"
    [ testFormats posixEpochFormats
    , testFormats mjdEpochFormats
    , checkCase posixEpoch q
    ]

testFormats :: Cases -> TestTree
testFormats c = testGroup (formatTime "%c" (_casesTime c)) $
    checkCase (_casesTime c) <$> _casesExpectations c

checkCase :: UTCTime -> (String, String, String) -> TestTree
checkCase t (e, f, m) = testCase m $ e @=? formatTime f t

data Cases = Cases
    { _casesTime :: !UTCTime
    , _casesExpectations :: ![(String, String, String)]
    }

-- -------------------------------------------------------------------------- --
-- Test Cases

-- TODO
--
q :: (String, String, String)
-- q = ("%q", "%q", "\"%q\" is not supported by Pact")
q = ("000000000000", "%q", "\"%q\" is not supported by Pact")

-- | 1970-01-01T00:00:00.000000Z
--
posixEpochFormats :: Cases
posixEpochFormats = Cases posixEpoch
    [ ("%", "%%", "literal \"%\"")
    , ("+0000", "%z", "RFC 822/ISO 8601:1988 style numeric time zone (e.g., \"-0600\" or \"+0100\")")
    , ("+00:00", "%N", "ISO 8601 style numeric time zone (e.g., \"-06:00\" or \"+01:00\") /EXTENSION/")
    , ("UTC", "%Z", "timezone name")
    , ("Thu Jan  1 00:00:00 UTC 1970", "%c", "same as \"%a %b %e %H:%M:%S %Z %Y\"")
    , ("00:00", "%R", "same as \"%H:%M\"")
    , ("00:00:00", "%T", "same as \"%H:%M:%S\"")
    , ("00:00:00", "%X", "same as \"%H:%M:%S\"")
    , ("12:00:00 AM", "%r", "same as \"%I:%M:%S %p\" (time12Fmt)")
    , ("am", "%P", "lowercase day-half of day (\"am\", or \"pm\")")
    , ("AM", "%p", "uppercase day-half of day (\"AM\", \"PM\")")
    , ("00", "%H", "hour of day (24-hour), 0-padded to two chars, \"00\"–\"23\"")
    , (" 0", "%k", "hour of day (24-hour), space-padded to two chars, \" 0\"–\"23\"")
    , ("12", "%I", "hour of day-half (12-hour), 0-padded to two chars, \"01\"–\"12\"")
    , ("12", "%l", "hour of day-half (12-hour), space-padded to two chars, \" 1\"–\"12\"")
    , ("00", "%M", "minute of hour, 0-padded to two chars, \"00\"–\"59\"")
    , ("00", "%S", "second of minute (without decimal part), 0-padded to two chars, \"00\"–\"60\"")
    , ("000000", "%v", "microsecond of second, 0-padded to six chars, \"000000\"–\"999999\". /EXTENSION/")
    , ("", "%Q", "decimal point and fraction of second, up to 6 second decimals, without trailing zeros. For a whole number of seconds, %Q produces the empty string. /EXTENSION/")
    , ("0", "%s", "number of whole seconds since the Unix epoch. For times before the Unix epoch, this is a negative number. Note that in %s.%q and %s%Q the decimals are positive, not negative. For example, 0.9 seconds before the Unix epoch is formatted as \"-1.1\" with %s%Q.")
    , ("01/01/70", "%D", "same as \"%m/%d/%y\"")
    , ("1970-01-01", "%F", "same as \"%Y-%m-%d\"")
    , ("01/01/70", "%x", "same as \"%m/%d/%y\" (dateFmt)")
    , ("1970", "%Y", "year, no padding.")
    , ("70", "%y", "year of century, 0-padded to two chars, \"00\"–\"99\"")
    , ("19", "%C", "century, no padding.")
    , ("January", "%B", "month name, long form (\"January\"–\"December\")")
    , ("Jan", "%b", "month name, short form (\"Jan\"–\"Dec\")")
    , ("Jan", "%h", "month name, short form (\"Jan\"–\"Dec\")")
    , ("01", "%m", "month of year, 0-padded to two chars, \"01\"–\"12\"")
    , ("01", "%d", "day of month, 0-padded to two chars, \"01\"–\"31\"")
    , (" 1", "%e", "day of month, space-padded to two chars, \" 1\"–\"31\"")
    , ("001", "%j", "day of year, 0-padded to three chars, \"001\"–\"366\"")
    , ("1970", "%G", "year for Week Date format, no padding.")
    , ("70", "%g", "year of century for Week Date format, 0-padded to two chars, \"00\"–\"99\"")
    , ("19", "%f", "century for Week Date format, no padding. /EXTENSION/")
    , ("01", "%V", "week of year for Week Date format, 0-padded to two chars, \"01\"–\"53\"")
    , ("4", "%u", "day of week for Week Date format, \"1\"–\"7\"")
    , ("Thu", "%a", "day of week, short form (\"Sun\"–\"Sat\")")
    , ("Thursday", "%A", "day of week, long form (\"Sunday\"–\"Saturday\")")
    , ("00", "%U", "week of year where weeks start on Sunday, 0-padded to two chars, \"00\"–\"53\"")
    , ("4", "%w", "day of week number, \"0\" (= Sunday) – \"6\" (= Saturday)")
    , ("00", "%W", "week of year where weeks start on Monday, 0-padded to two chars, \"00\"–\"53\"")
    ]

-- | 1858-11-17T00:00:00.000000Z
--
mjdEpochFormats :: Cases
mjdEpochFormats = Cases mjdEpoch
    [ ("%", "%%", "literal \"%\"")
    , ("+0000", "%z", "RFC 822/ISO 8601:1988 style numeric time zone (e.g., \"-0600\" or \"+0100\")")
    , ("+00:00", "%N", "ISO 8601 style numeric time zone (e.g., \"-06:00\" or \"+01:00\") /EXTENSION/")
    , ("UTC", "%Z", "timezone name")
    , ("Wed Nov 17 00:00:00 UTC 1858", "%c", "same as \"%a %b %e %H:%M:%S %Z %Y\"")
    , ("00:00", "%R", "same as \"%H:%M\"")
    , ("00:00:00", "%T", "same as \"%H:%M:%S\"")
    , ("00:00:00", "%X", "same as \"%H:%M:%S\"")
    , ("12:00:00 AM", "%r", "same as \"%I:%M:%S %p\" (time12Fmt)")
    , ("am", "%P", "lowercase day-half of day (\"am\", or \"pm\")")
    , ("AM", "%p", "uppercase day-half of day (\"AM\", \"PM\")")
    , ("00", "%H", "hour of day (24-hour), 0-padded to two chars, \"00\"–\"23\"")
    , (" 0", "%k", "hour of day (24-hour), space-padded to two chars, \" 0\"–\"23\"")
    , ("12", "%I", "hour of day-half (12-hour), 0-padded to two chars, \"01\"–\"12\"")
    , ("12", "%l", "hour of day-half (12-hour), space-padded to two chars, \" 1\"–\"12\"")
    , ("00", "%M", "minute of hour, 0-padded to two chars, \"00\"–\"59\"")
    , ("00", "%S", "second of minute (without decimal part), 0-padded to two chars, \"00\"–\"60\"")
    , ("000000", "%v", "microsecond of second, 0-padded to six chars, \"000000\"–\"999999\". /EXTENSION/")
    , ("", "%Q", "decimal point and fraction of second, up to 6 second decimals, without trailing zeros. For a whole number of seconds, %Q produces the empty string. /EXTENSION/")
    , ("-3506716800", "%s", "number of whole seconds since the Unix epoch. For times before the Unix epoch, this is a negative number. Note that in %s.%q and %s%Q the decimals are positive, not negative. For example, 0.9 seconds before the Unix epoch is formatted as \"-1.1\" with %s%Q.")
    , ("11/17/58", "%D", "same as \"%m/%d/%y\"")
    , ("1858-11-17", "%F", "same as \"%Y-%m-%d\"")
    , ("11/17/58", "%x", "same as \"%m/%d/%y\" (dateFmt)")
    , ("1858", "%Y", "year, no padding.")
    , ("58", "%y", "year of century, 0-padded to two chars, \"00\"–\"99\"")
    , ("18", "%C", "century, no padding.")
    , ("November", "%B", "month name, long form (\"January\"–\"December\")")
    , ("Nov", "%b", "month name, short form (\"Jan\"–\"Dec\")")
    , ("Nov", "%h", "month name, short form (\"Jan\"–\"Dec\")")
    , ("11", "%m", "month of year, 0-padded to two chars, \"01\"–\"12\"")
    , ("17", "%d", "day of month, 0-padded to two chars, \"01\"–\"31\"")
    , ("17", "%e", "day of month, space-padded to two chars, \" 1\"–\"31\"")
    , ("321", "%j", "day of year, 0-padded to three chars, \"001\"–\"366\"")
    , ("1858", "%G", "year for Week Date format, no padding.")
    , ("58", "%g", "year of century for Week Date format, 0-padded to two chars, \"00\"–\"99\"")
    , ("18", "%f", "century for Week Date format, no padding. /EXTENSION/")
    , ("46", "%V", "week of year for Week Date format, 0-padded to two chars, \"01\"–\"53\"")
    , ("3", "%u", "day of week for Week Date format, \"1\"–\"7\"")
    , ("Wed", "%a", "day of week, short form (\"Sun\"–\"Sat\")")
    , ("Wednesday", "%A", "day of week, long form (\"Sunday\"–\"Saturday\")")
    , ("46", "%U", "week of year where weeks start on Sunday, 0-padded to two chars, \"00\"–\"53\"")
    , ("3", "%w", "day of week number, \"0\" (= Sunday) – \"6\" (= Saturday)")
    , ("46", "%W", "week of year where weeks start on Monday, 0-padded to two chars, \"00\"–\"53\"")
    ]

-- | https://pact-language.readthedocs.io/en/stable/pact-reference.html#time-formats
--
-- Can be used as a tempalte to define new test cases
--
formats :: [(String, String)]
formats =
    [ ("%%", "literal \"%\"")
    , ("%z", "RFC 822/ISO 8601:1988 style numeric time zone (e.g., \"-0600\" or \"+0100\")")
    , ("%N", "ISO 8601 style numeric time zone (e.g., \"-06:00\" or \"+01:00\") /EXTENSION/")
    , ("%Z", "timezone name")
    , ("%c", "same as \"%a %b %e %H:%M:%S %Z %Y\"")
    , ("%R", "same as \"%H:%M\"")
    , ("%T", "same as \"%H:%M:%S\"")
    , ("%X", "same as \"%H:%M:%S\"")
    , ("%r", "same as \"%I:%M:%S %p\" (time12Fmt)")
    , ("%P", "lowercase day-half of day (\"am\", or \"pm\")")
    , ("%p", "uppercase day-half of day (\"AM\", \"PM\")")
    , ("%H", "hour of day (24-hour), 0-padded to two chars, \"00\"–\"23\"")
    , ("%k", "hour of day (24-hour), space-padded to two chars, \" 0\"–\"23\"")
    , ("%I", "hour of day-half (12-hour), 0-padded to two chars, \"01\"–\"12\"")
    , ("%l", "hour of day-half (12-hour), space-padded to two chars, \" 1\"–\"12\"")
    , ("%M", "minute of hour, 0-padded to two chars, \"00\"–\"59\"")
    , ("%S", "second of minute (without decimal part), 0-padded to two chars, \"00\"–\"60\"")
    , ("%v", "microsecond of second, 0-padded to six chars, \"000000\"–\"999999\". /EXTENSION/")
    , ("%Q", "decimal point and fraction of second, up to 6 second decimals, without trailing zeros. For a whole number of seconds, %Q produces the empty string. /EXTENSION/")
    , ("%s", "number of whole seconds since the Unix epoch. For times before the Unix epoch, this is a negative number. Note that in %s.%q and %s%Q the decimals are positive, not negative. For example, 0.9 seconds before the Unix epoch is formatted as \"-1.1\" with %s%Q.")
    , ("%D", "same as \"%m/%d/%y\"")
    , ("%F", "same as \"%Y-%m-%d\"")
    , ("%x", "same as \"%m/%d/%y\" (dateFmt)")
    , ("%Y", "year, no padding.")
    , ("%y", "year of century, 0-padded to two chars, \"00\"–\"99\"")
    , ("%C", "century, no padding.")
    , ("%B", "month name, long form (\"January\"–\"December\")")
    , ("%b", "month name, short form (\"Jan\"–\"Dec\")")
    , ("%h", "month name, short form (\"Jan\"–\"Dec\")")
    , ("%m", "month of year, 0-padded to two chars, \"01\"–\"12\"")
    , ("%d", "day of month, 0-padded to two chars, \"01\"–\"31\"")
    , ("%e", "day of month, space-padded to two chars, \" 1\"–\"31\"")
    , ("%j", "day of year, 0-padded to three chars, \"001\"–\"366\"")
    , ("%G", "year for Week Date format, no padding.")
    , ("%g", "year of century for Week Date format, 0-padded to two chars, \"00\"–\"99\"")
    , ("%f", "century for Week Date format, no padding. /EXTENSION/")
    , ("%V", "week of year for Week Date format, 0-padded to two chars, \"01\"–\"53\"")
    , ("%u", "day of week for Week Date format, \"1\"–\"7\"")
    , ("%a", "day of week, short form (\"Sun\"–\"Sat\")")
    , ("%A", "day of week, long form (\"Sunday\"–\"Saturday\")")
    , ("%U", "week of year where weeks start on Sunday, 0-padded to two chars, \"00\"–\"53\"")
    , ("%w", "day of week number, \"0\" (= Sunday) – \"6\" (= Saturday)")
    , ("%W", "week of year where weeks start on Monday, 0-padded to two chars, \"00\"–\"53\"")
    -- , "%q" -- (picoseconds, zero-padded) does not work properly so not documented here.
    ]
