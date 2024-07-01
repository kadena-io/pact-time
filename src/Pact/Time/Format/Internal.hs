{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Pact.Time.Format.Internal
-- Copyright: Copyright © 2021 Kadena LLC.
--                      © 2013−2014 Liyang HU Liyang HU
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- The code in this module is derived from various modules of the thyme package,
-- which is copyright (c) 2013 Liyang HU and distributed under a BSD3 license.
--
module Pact.Time.Format.Internal
( formatTime
, parseTime
, readTime
, readsTime
) where

import Control.Applicative
import Control.Monad.State.Strict

import Data.Aeson (FromJSON(..), ToJSON(..), withText, Value(String))
import Data.Attoparsec.ByteString.Char8 (Parser, Result, IResult (..))
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.Bits
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L
import Data.Char
import Data.Int
import qualified Data.Text as T
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector.Unboxed as VU

import Lens.Micro

-- internal modules

import Pact.Time.Internal
import Pact.Time.Format.Locale

-- -------------------------------------------------------------------------- --
-- Lens Utils (from microlens-mtl)

infix 4 .=

(.=) :: MonadState s m => ASetter s s a b -> b -> m ()
l .= x = modify (l .~ x)
{-# INLINE (.=) #-}

assign :: MonadState s m => ASetter s s a b -> b -> m ()
assign l x = l .= x
{-# INLINE assign #-}

-- -------------------------------------------------------------------------- --
-- Misc Utils

shows02 :: Int -> ShowS
shows02 n = if n < 10 then (:) '0' . shows n else shows n
{-# INLINE shows02 #-}

shows_2 :: Int -> ShowS
shows_2 n = if n < 10 then (:) ' ' . shows n else shows n
{-# INLINE shows_2 #-}

shows03 :: Int -> ShowS
shows03 n
    | n < 10 = (++) "00" . shows n
    | n < 100 = (++) "0" . shows n
    | otherwise = shows n
{-# INLINE shows03 #-}

showsYear :: Int -> ShowS
showsYear n@(abs -> u)
    | u < 10 = neg . (++) "000" . shows u
    | u < 100 = neg . (++) "00" . shows u
    | u < 1000 = neg . (++) "0" . shows u
    | otherwise = neg . shows u
  where
    neg = if n < 0 then (:) '-' else id
{-# INLINE showsYear #-}

fills06 :: Micros -> ShowS
fills06 n
    | n < 10 = (++) "00000"
    | n < 100 = (++) "0000"
    | n < 1000 = (++) "000"
    | n < 10000 = (++) "00"
    | n < 100000 = (++) "0"
    | otherwise = id
{-# INLINE fills06 #-}

drops0 :: Micros -> ShowS
drops0 n = case divMod n 10 of
    (q, 0) -> drops0 q
    _ -> shows n
{-# INLINE drops0 #-}

-- -------------------------------------------------------------------------- --
-- Misc Types

-- Unbounded
type UnboundedInt = Int

type Minutes = UnboundedInt
type Days = UnboundedInt
type Year = UnboundedInt
type Century = UnboundedInt

-- Bounded
type BoundedInt = Int

type Hour = BoundedInt
type Minute = BoundedInt
type Second = BoundedInt
type Month = BoundedInt
type DayOfMonth = BoundedInt
type DayOfYear = BoundedInt
type DayOfWeek = BoundedInt
type WeekOfYear = BoundedInt

-- -------------------------------------------------------------------------- --
-- Year Month Day

data YearMonthDay = YearMonthDay
    { _ymdYear :: {-# UNPACK #-} !Year
    , _ymdMonth :: {-# UNPACK #-} !Month
    , _ymdDay :: {-# UNPACK #-} !DayOfMonth
    }

ymdFromOrdinal :: OrdinalDate -> YearMonthDay
ymdFromOrdinal (OrdinalDate y yd) = YearMonthDay y m d
  where
    MonthDay m d = monthDaysFromDayOfYear (isLeapYear y) yd
{-# INLINEABLE ymdFromOrdinal #-}

ymdToOrdinal :: YearMonthDay -> OrdinalDate
ymdToOrdinal (YearMonthDay y m d) = OrdinalDate y $
    monthDaysToDayOfYear (isLeapYear y) (MonthDay m d)

{-# INLINEABLE ymdToOrdinal #-}

toGregorian :: YearMonthDay -> ModifiedJulianDay
toGregorian = fromOrdinalDate . ymdToOrdinal
{-# INLINEABLE toGregorian #-}

-- -------------------------------------------------------------------------- --
-- Ordinal Dates

data OrdinalDate = OrdinalDate
    { _odYear :: {-# UNPACK #-} !Year
    , _odDay :: {-# UNPACK #-} !DayOfYear
    }

-- | Gregorian leap year?
isLeapYear :: Year -> Bool
isLeapYear y = y .&. 3 == 0 && (r100 /= 0 || q100 .&. 3 == 0)
  where
    (q100, r100) = y `quotRem` 100

toOrdinalDate :: ModifiedJulianDay -> OrdinalDate
toOrdinalDate (ModifiedJulianDay mjd)
    | dayB0 <= 0 = case toOrdB0 dayInQC of
        OrdinalDate y yd -> OrdinalDate (y + quadCent * 400) yd
    | otherwise = toOrdB0 dayB0
  where
    dayB0 = mjd + 678575
    (quadCent, dayInQC) = dayB0 `divMod` 146097

    -- Input: days since 1-1-1. Precondition: has to be positive!
    toOrdB0 dB0 = res
      where
        (y0, r) = (400 * dB0) `quotRem` 146097
        d0 = dayInYear y0 dB0
        d1 = dayInYear (y0 + 1) dB0
        res = if r > 146097 - 600 && d1 > 0
                then OrdinalDate (y0 + 1 + 1) d1
                else OrdinalDate (y0 + 1) d0
    {-# INLINE toOrdB0 #-}

    -- Input: (year - 1) (day as days since 1-1-1)
    -- Precondition: year is positive!
    dayInYear y0 dB0 = dB0 - 365 * y0 - leaps + 1
      where
        leaps = y0 `shiftR` 2 - centuries + centuries `shiftR` 2
        centuries = y0 `quot` 100
    {-# INLINE dayInYear #-}
{-# INLINEABLE toOrdinalDate #-}


fromOrdinalDate :: OrdinalDate -> ModifiedJulianDay
fromOrdinalDate (OrdinalDate year yd) = ModifiedJulianDay mjd
  where
    years = year - 1
    centuries = years `div` 100
    leaps = years `shiftR` 2 - centuries + centuries `shiftR` 2
    mjd = 365 * years + leaps - 678576
        + clip 1 (if isLeapYear year then 366 else 365) yd
    clip a b = max a . min b
{-# INLINEABLE fromOrdinalDate #-}

-- -------------------------------------------------------------------------- --
-- Months

monthLengths :: VU.Vector Days
monthLengths     = VU.fromList [31,28,31,30,31,30,31,31,30,31,30,31]
{-# NOINLINE monthLengths #-}

monthLengthsLeap :: VU.Vector Days
monthLengthsLeap = VU.fromList [31,29,31,30,31,30,31,31,30,31,30,31]
{-# NOINLINE monthLengthsLeap #-}

monthDays :: VU.Vector ({-Month-}Int8, {-DayOfMonth-}Int8)
monthDays = VU.generate 365 go
  where
    dom01 = VU.prescanl' (+) 0 monthLengths
    go yd = (fromIntegral m, fromIntegral d)
      where
        m = maybe 12 id $ VU.findIndex (yd <) dom01
        d = succ yd - VU.unsafeIndex dom01 (pred m)
{-# NOINLINE monthDays #-}

monthDaysLeap :: VU.Vector ({-Month-}Int8, {-DayOfMonth-}Int8)
monthDaysLeap = VU.generate 366 go
  where
    dom01 = VU.prescanl' (+) 0 monthLengthsLeap
    go yd = (fromIntegral m, fromIntegral d)
      where
        m = maybe 12 id $ VU.findIndex (yd <) dom01
        d = succ yd - VU.unsafeIndex dom01 (pred m)
{-# NOINLINE monthDaysLeap #-}

data MonthDay = MonthDay
    { _mdMonth :: {-# UNPACK #-} !Month
    , _mdDay :: {-# UNPACK #-} !DayOfMonth
    }

monthDaysFromDayOfYear :: Bool -> DayOfYear -> MonthDay
monthDaysFromDayOfYear leap yd = MonthDay m d
  where
    i = max 0 . min lastDay $ pred yd
    (fromIntegral -> m, fromIntegral -> d) = VU.unsafeIndex table i
    (lastDay, table) = if leap
      then (365, monthDaysLeap)
      else (364, monthDays)
{-# INLINE monthDaysFromDayOfYear #-}

monthDaysToDayOfYear :: Bool -> MonthDay -> DayOfYear
monthDaysToDayOfYear leap (MonthDay month mday) = div (367 * m - 362) 12 + k + d
  where
    m = max 1 . min 12 $ month
    l = VU.unsafeIndex lengths (pred m)
    d = max 1 . min l $ mday
    k = if m <= 2 then 0 else ok

    (lengths, ok) = if leap
      then (monthLengthsLeap, -1)
      else (monthLengths, -2)
{-# INLINE monthDaysToDayOfYear #-}

-- -------------------------------------------------------------------------- --
-- Week Date

data WeekDate = WeekDate
    { _wdYear :: {-# UNPACK #-} !Year
    , _wdWeek :: {-# UNPACK #-} !WeekOfYear
    , _wdDay :: {-# UNPACK #-} !DayOfWeek
    }

toWeekDate :: ModifiedJulianDay -> WeekDate
toWeekDate d = toWeekOrdinal (toOrdinalDate d) d
{-# INLINEABLE toWeekDate #-}

fromWeekDate :: WeekDate -> ModifiedJulianDay
fromWeekDate wd@(WeekDate y _ _) = fromWeekLast (lastWeekOfYear y) wd
{-# INLINEABLE fromWeekDate #-}

toWeekOrdinal :: OrdinalDate -> ModifiedJulianDay -> WeekDate
toWeekOrdinal (OrdinalDate y0 yd) (ModifiedJulianDay mjd) =
    WeekDate y1 (w1 + 1) (d7mod + 1)
  where
    -- pilfered and refactored; no idea what foo and bar mean
    d = mjd + 2
    (d7div, d7mod) = divMod d 7

    -- foo :: Year -> {-WeekOfYear-1-}Int
    foo y = bar $ fromOrdinalDate $ OrdinalDate y 6

    -- bar :: ModifiedJulianDay -> {-WeekOfYear-1-}Int
    bar (ModifiedJulianDay k) = d7div - div k 7

    w0 = bar $ ModifiedJulianDay (d - yd + 4)
    (y1, w1) = case w0 of
        -1 -> (y0 - 1, foo (y0 - 1))
        52 | foo (y0 + 1) == 0 -> (y0 + 1, 0)
        _ -> (y0, w0)
{-# INLINE toWeekOrdinal #-}

lastWeekOfYear :: Year -> WeekOfYear
lastWeekOfYear y = if _wdWeek wd == 53 then 53 else 52
  where
    wd = toWeekDate $ fromOrdinalDate $ OrdinalDate y 365
{-# INLINE lastWeekOfYear #-}

fromWeekLast :: WeekOfYear -> WeekDate -> ModifiedJulianDay
fromWeekLast wMax (WeekDate y w d) = ModifiedJulianDay mjd
  where
    -- pilfered and refactored
    ModifiedJulianDay k = fromOrdinalDate $ OrdinalDate y 6
    mjd = k - mod k 7 - 10 + clip 1 7 d + clip 1 wMax w * 7
    clip a b = max a . min b
{-# INLINE fromWeekLast #-}

-- -------------------------------------------------------------------------- --
-- Sunday Weeks

-- | Weeks numbered from 0 to 53, starting with the first Sunday of the year
-- as the first day of week 1. The last week of a given year and week 0 of
-- the next both refer to the same week, but not all 'DayOfWeek' are valid.
-- 'Year' coincides with that of 'gregorian'.
--
data SundayWeek = SundayWeek
    { _swYear :: {-# UNPACK #-} !Year
    , _swWeek :: {-# UNPACK #-} !WeekOfYear
    , _swDay :: {-# UNPACK #-} !DayOfWeek
    }

fromSundayWeek :: SundayWeek -> ModifiedJulianDay
fromSundayWeek (SundayWeek y w d) = ModifiedJulianDay (firstDay + yd)
  where
    ModifiedJulianDay firstDay = fromOrdinalDate $ OrdinalDate y 1
    -- following are all 0-based year days
    firstSunday = mod (4 - firstDay) 7
    yd = firstSunday + 7 * (w - 1) + d
{-# INLINEABLE fromSundayWeek #-}

toSundayOrdinal :: OrdinalDate -> ModifiedJulianDay -> SundayWeek
toSundayOrdinal (OrdinalDate y yd) (ModifiedJulianDay mjd) =
    SundayWeek y (d7div - div k 7) d7mod
  where
    d = mjd + 3
    k = d - yd
    (d7div, d7mod) = divMod d 7
{-# INLINE toSundayOrdinal #-}

-- -------------------------------------------------------------------------- --
-- Monaday Weeks

-- | Weeks numbered from 0 to 53, starting with the first Monday of the year
-- as the first day of week 1. The last week of a given year and week 0 of
-- the next both refer to the same week, but not all 'DayOfWeek' are valid.
-- 'Year' coincides with that of 'gregorian'.
--
data MondayWeek = MondayWeek
    { _mwYear :: {-# UNPACK #-} !Year
    , _mwWeek :: {-# UNPACK #-} !WeekOfYear
    , _mwDay :: {-# UNPACK #-} !DayOfWeek
    }

fromMondayWeek :: MondayWeek -> ModifiedJulianDay
fromMondayWeek (MondayWeek y w d) = ModifiedJulianDay (firstDay + yd)
  where
    ModifiedJulianDay firstDay = fromOrdinalDate $ OrdinalDate y 1
    -- following are all 0-based year days
    firstMonday = mod (5 - firstDay) 7
    yd = firstMonday + 7 * (w - 1) + d - 1
{-# INLINEABLE fromMondayWeek #-}

toMondayOrdinal :: OrdinalDate -> ModifiedJulianDay -> MondayWeek
toMondayOrdinal (OrdinalDate y yd) (ModifiedJulianDay mjd) =
    MondayWeek y (d7div - div k 7) (d7mod + 1)
  where
    d = mjd + 2
    k = d - yd
    (d7div, d7mod) = divMod d 7
{-# INLINE toMondayOrdinal #-}

-- -------------------------------------------------------------------------- --
-- Time Of Day

data TimeOfDay = TimeOfDay
    { _todHour :: {-# UNPACK #-} !Hour
    , _todMin :: {-# UNPACK #-} !Minute
    , _todSec :: {-# UNPACK #-} !NominalDiffTime
    }

timeOfDayFromNominalDiffTime :: NominalDiffTime -> TimeOfDay
timeOfDayFromNominalDiffTime (NominalDiffTime t) = TimeOfDay
    (fromIntegral h) (fromIntegral m) (NominalDiffTime s)
  where
    (h, ms) = quotRem t 3600000000
    (m, s) = quotRem ms 60000000
{-# INLINEABLE timeOfDayFromNominalDiffTime #-}

-- -------------------------------------------------------------------------- --
-- Format Time

class FormatTime t where
    showsTime :: t -> (Char -> ShowS) -> Char -> ShowS

formatTime :: (FormatTime t) => String -> t -> String
formatTime spec t = formatTimeS spec t ""
{-# INLINEABLE formatTime #-}

formatTimeS :: (FormatTime t) => String -> t -> ShowS
formatTimeS spec t = go spec
  where
    -- leave unrecognised codes as they are
    format = showsTime t (\c s -> '%' : c : s)
    go s = case s of
        '%' : c : rest -> case c of
            -- aggregate
            'c' -> go (dateTimeFmt l ++ rest)
            'r' -> go (time12Fmt l ++ rest)
            'X' -> go (timeFmt l ++ rest)
            'x' -> go (dateFmt l ++ rest)
            -- modifier (whatever)
            '-' -> go ('%' : rest)
            '_' -> go ('%' : rest)
            '0' -> go ('%' : rest)
            '^' -> go ('%' : rest)
            '#' -> go ('%' : rest)
            -- escape (why would anyone need %t and %n?)
            '%' -> (:) '%' . go rest
            -- default
            _ -> format c . go rest
        c : rest -> (:) c . go rest
        [] -> id
      where
        l = defaultTimeLocale
{-# INLINEABLE formatTimeS #-}

instance FormatTime TimeOfDay where
    showsTime (TimeOfDay h m (NominalDiffTime s)) = \ def c -> case c of
        -- aggregate
        'R' -> shows02 h . (:) ':' . shows02 m
        'T' -> shows02 h . (:) ':' . shows02 m . (:) ':' . shows02 si
        -- AM/PM
        'P' -> (++) $ toLower <$> if h < 12 then fst amPm else snd amPm
        'p' -> (++) $ if h < 12 then fst amPm else snd amPm
        -- Hour
        'H' -> shows02 h
        'I' -> shows02 $ 1 + mod (h - 1) 12
        'k' -> shows_2 h
        'l' -> shows_2 $ 1 + mod (h - 1) 12
        -- Minute
        'M' -> shows02 m
        -- Second
        'S' -> shows02 si

        -- TODO: Unsupported by Pact
        'q' -> fills06 su . shows su . (++) "000000"

        'v' -> fills06 su . shows su
        'Q' -> if su == 0 then id else (:) '.' . fills06 su . drops0 su
        -- default
        _ -> def c
      where
        (fromIntegral -> si, su) = quotRem s 1000000
        TimeLocale {..} = defaultTimeLocale
    {-# INLINEABLE showsTime #-}

instance FormatTime SundayWeek where
    showsTime (SundayWeek y w d) = \ def c -> case c of
        -- Year
        'Y' -> showsYear y
        'y' -> shows02 (mod y 100)
        'C' -> shows02 (div y 100)
        -- WeekOfYear
        'U' -> shows02 w
        -- DayOfWeek
        'u' -> shows $ if d == 0 then 7 else d
        'w' -> shows $ if d == 7 then 0 else d
        'A' -> (++) . fst $ wDays !! mod d 7
        'a' -> (++) . snd $ wDays !! mod d 7
        -- default
        _ -> def c
      where
        TimeLocale {..} = defaultTimeLocale
    {-# INLINEABLE showsTime #-}

instance FormatTime MondayWeek where
    showsTime (MondayWeek y w d) = \ def c -> case c of
        -- Year
        'Y' -> showsYear y
        'y' -> shows02 (mod y 100)
        'C' -> shows02 (div y 100)
        -- WeekOfYear
        'W' -> shows02 w
        -- DayOfWeek
        'u' -> shows $ if d == 0 then 7 else d
        'w' -> shows $ if d == 7 then 0 else d
        'A' -> (++) . fst $ wDays !! mod d 7
        'a' -> (++) . snd $ wDays !! mod d 7
        -- default
        _ -> def c
      where
        TimeLocale {..} = defaultTimeLocale
    {-# INLINEABLE showsTime #-}

instance FormatTime WeekDate where
    showsTime (WeekDate y w d) = \ def c -> case c of
        -- Year
        'G' -> showsYear y
        'g' -> shows02 (mod y 100)
        'f' -> shows02 (div y 100)
        -- WeekOfYear
        'V' -> shows02 w
        -- DayOfWeek
        'u' -> shows $ if d == 0 then 7 else d
        'w' -> shows $ if d == 7 then 0 else d
        'A' -> (++) . fst $ wDays !! mod d 7
        'a' -> (++) . snd $ wDays !! mod d 7
        -- default
        _ -> def c
      where
        TimeLocale {..} = defaultTimeLocale
    {-# INLINEABLE showsTime #-}

instance FormatTime YearMonthDay where
    showsTime (YearMonthDay y m d) def c = case c of
        -- aggregate
        'D' -> shows02 m . (:) '/' . shows02 d . (:) '/' . shows02 (mod y 100)
        'F' -> showsYear y . (:) '-' . shows02 m . (:) '-' . shows02 d
        -- Year
        'Y' -> showsYear y
        'y' -> shows02 (mod y 100)
        'C' -> shows02 (div y 100)
        -- Month
        'B' -> (++) . fst $ months !! (m - 1)
        'b' -> (++) . snd $ months !! (m - 1)
        'h' -> (++) . snd $ months !! (m - 1)
        'm' -> shows02 m
        -- DayOfMonth
        'd' -> shows02 d
        'e' -> shows_2 d
        -- default
        _ -> def c
      where
        TimeLocale {..} = defaultTimeLocale
    {-# INLINEABLE showsTime #-}

instance FormatTime MonthDay where
    showsTime (MonthDay m d) def c = case c of
        -- Month
        'B' -> (++) . fst $ months !! (m - 1)
        'b' -> (++) . snd $ months !! (m - 1)
        'h' -> (++) . snd $ months !! (m - 1)
        'm' -> shows02 m
        -- DayOfMonth
        'd' -> shows02 d
        'e' -> shows_2 d
        -- default
        _ -> def c
      where
        TimeLocale {..} = defaultTimeLocale
    {-# INLINEABLE showsTime #-}

instance FormatTime OrdinalDate where
    showsTime (OrdinalDate y d) def c = case c of
        -- Year
        'Y' -> showsYear y
        'y' -> shows02 (mod y 100)
        'C' -> shows02 (div y 100)
        -- DayOfYear
        'j' -> shows03 d
        -- default
        _ -> def c
    {-# INLINEABLE showsTime #-}

-- | Format Date that is represented as 'ModifiedJulianDay'
--
instance FormatTime ModifiedJulianDay where
    showsTime d@(toOrdinalDate -> ordinal)
        = showsTime ordinal
        . showsTime (ymdFromOrdinal ordinal)
        . showsTime (toWeekOrdinal ordinal d)
        . showsTime (toSundayOrdinal ordinal d)
        . showsTime (toMondayOrdinal ordinal d)
    {-# INLINEABLE showsTime #-}

instance FormatTime ModifiedJulianDate where
    showsTime (ModifiedJulianDate d dt) =
        showsTime d . showsTime (timeOfDayFromNominalDiffTime dt)
    {-# INLINEABLE showsTime #-}

instance FormatTime UTCTime where
    showsTime t def c = case c of
        's' -> shows . fst $ quotRem (toPosixTimestampMicros t) 1000000
        _ -> (showsTime (toModifiedJulianDate t) . formatUtcZone) def c
    {-# INLINEABLE showsTime #-}

-- | Pact only supports UTC
--
formatUtcZone :: (Char -> ShowS) -> Char -> ShowS
formatUtcZone def c = case c of
    'z' -> (++) "+0000"
    'N' -> (++) "+00:00"
    'Z' -> (++) "UTC"
    _ -> def c
{-# INLINEABLE formatUtcZone #-}

-- -------------------------------------------------------------------------- --
-- Parser Utils

utf8Char :: Char -> S.ByteString
utf8Char = L.toStrict . B.toLazyByteString . B.charUtf8
{-# INLINE utf8Char #-}

utf8String :: String -> S.ByteString
utf8String = L.toStrict . B.toLazyByteString . B.stringUtf8
{-# INLINE utf8String #-}

parserToReadS :: Parser a -> ReadS a
parserToReadS = go . P.parse
  where
    go :: (S.ByteString -> Result a) -> ReadS a
    go k (splitAt 32 -> (h, t)) = case k (utf8String h) of
        -- `date -R | wc -c` is 32 characters
        Fail rest cxts msg -> fail $ concat [ "parserToReadS: ", msg
            , "; remaining: ", show (utf8Decode rest), "; stack: ", show cxts ]
        Partial k' -> go k' t
        Done rest a -> return (a, utf8Decode rest ++ t)
    {-# INLINEABLE go #-}

    utf8Decode :: S.ByteString -> String
    utf8Decode = Text.unpack . Text.decodeUtf8
    {-# INLINE utf8Decode #-}
{-# INLINEABLE parserToReadS #-}

indexOfCI :: [String] -> Parser Int
indexOfCI = P.choice . zipWith (\i s -> i <$ stringCI s) [0..]
{-# INLINE indexOfCI #-}

-- | Case-insensitive UTF-8 ByteString parser
--
-- Matches one character at a time. Slow.
--
stringCI :: String -> Parser ()
stringCI = foldl (\p c -> p *> charCI c) (pure ())
{-# INLINE stringCI #-}

-- | Case-insensitive UTF-8 ByteString parser
--
-- We can't easily perform upper/lower case conversion on the input, so
-- instead we accept either one of @toUpper c@ and @toLower c@.
--
charCI :: Char -> Parser ()
charCI c = if u == l then charU8 c else charU8 l <|> charU8 u
  where
    l = toLower c
    u = toUpper c
{-# INLINE charCI #-}

charU8 :: Char -> Parser ()
charU8 c = () <$ P.string (utf8Char c)
{-# INLINE charU8 #-}

-- | Number may be prefixed with '-'
--
negative :: (Integral n) => Parser n -> Parser n
negative p = ($) <$> (negate <$ P.char '-' <|> pure id) <*> p
{-# INLINE negative #-}

-- | Fixed-length 0-padded decimal
--
dec0 :: Int -> Parser Int
dec0 n = either fail return . P.parseOnly P.decimal =<< P.take n
{-# INLINE dec0 #-}

-- | Fixed-length space-padded decimal
--
dec_ :: Int -> Parser Int
dec_ n = P.take n >>= either fail return
    . P.parseOnly P.decimal
    . S.dropWhile isSpace
{-# INLINE dec_ #-}

-- -------------------------------------------------------------------------- --
-- Time Zones

data TimeZone = TimeZone
    { _timeZoneMinutes :: {-# UNPACK #-} !Minutes
    , _timeZoneSummerOnly :: !Bool
    , _timeZoneName :: String
    }

timeZoneMinutes :: Lens' TimeZone Minutes
timeZoneMinutes = lens _timeZoneMinutes $ \a b -> a { _timeZoneMinutes = b }
{-# INLINE timeZoneMinutes #-}

utc :: TimeZone
utc = TimeZone 0 False "UTC"

timeZoneOffset :: TimeZone -> NominalDiffTime
timeZoneOffset = fromMicroseconds . fromIntegral . (*) 60000000 . negate . _timeZoneMinutes
{-# INLINE timeZoneOffset #-}

-- -------------------------------------------------------------------------- --
-- Parse String into a Time Parse Value

data TimeFlag
    = PostMeridiem
    | TwelveHour
    | HasCentury
    | IsPOSIXTime
    | IsOrdinalDate
    | IsGregorian
    | IsWeekDate
    | IsSundayWeek
    | IsMondayWeek
    deriving (Enum, Show)

data TimeParse = TimeParse
    { _tpCentury :: {-# UNPACK #-} !Century
    , _tpCenturyYear :: {-# UNPACK #-} !Int{-YearOfCentury-}
    , _tpMonth :: {-# UNPACK #-} !Month
    , _tpWeekOfYear :: {-# UNPACK #-} !WeekOfYear
    , _tpDayOfMonth :: {-# UNPACK #-} !DayOfMonth
    , _tpDayOfYear :: {-# UNPACK #-} !DayOfYear
    , _tpDayOfWeek :: {-# UNPACK #-} !DayOfWeek
    , _tpFlags :: {-# UNPACK #-} !Int{-BitSet TimeFlag-}
    , _tpHour :: {-# UNPACK #-} !Hour
    , _tpMinute :: {-# UNPACK #-} !Minute
    , _tpSecond :: {-# UNPACK #-} !Second
    , _tpSecFrac :: {-# UNPACK #-} !NominalDiffTime
    , _tpPOSIXTime :: {-# UNPACK #-} !NominalDiffTime
    , _tpTimeZone :: !TimeZone
    }

flag :: TimeFlag -> Lens' TimeParse Bool
flag (fromEnum -> f) = tpFlags . lens
    (`testBit` f) (\ n b -> (if b then setBit else clearBit) n f)
{-# INLINE flag #-}

tpYear :: TimeParse -> Year
tpYear tp = _tpCenturyYear tp + 100 * if tp ^. flag HasCentury
  then _tpCentury tp
  else if _tpCenturyYear tp < 69
    then 20
    else 19
{-# INLINE tpYear #-}

-- | Time 'Parser' for UTF-8 encoded 'ByteString's.
--
-- Attoparsec easily beats any 'String' parser out there, but we do have to
-- be careful to convert the input to UTF-8 'ByteString's.
--
timeParser :: String -> Parser TimeParse
timeParser = flip execStateT unixEpoch . go
  where

    go :: String -> StateT TimeParse Parser ()
    go spec = case spec of
        '%' : cspec : rspec -> case cspec of
            -- aggregate
            'c' -> go (dateTimeFmt l ++ rspec)
            'r' -> go (time12Fmt l ++ rspec)
            'X' -> go (timeFmt l ++ rspec)
            'x' -> go (dateFmt l ++ rspec)
            'R' -> go ("%H:%M" ++ rspec)
            'T' -> go ("%H:%M:%S" ++ rspec)
            'D' -> go ("%m/%d/%y" ++ rspec)
            'F' -> go ("%Y-%m-%d" ++ rspec)
            -- AM/PM
            'P' -> dayHalf
            'p' -> dayHalf
            -- Hour
            'H' -> lift (dec0 2) >>= setHour24
            'I' -> lift (dec0 2) >>= setHour12
            'k' -> (lift (dec_ 2) >>= setHour24)
                <|> (lift (dec_ 1) >>= setHour24)
            'l' -> (lift (dec_ 2) >>= setHour12)
                <|> (lift (dec_ 1) >>= setHour12)
            -- Minute
            'M' -> lift (dec0 2) >>= assign tpMinute >> go rspec
            -- Second
            'S' -> lift (dec0 2) >>= assign tpSecond >> go rspec

            -- TODO: Unsupported by pact
            'q' -> lift micro >>= assign tpSecFrac . NominalDiffTime >> go rspec

            'v' -> lift micro >>= assign tpSecFrac . NominalDiffTime >> go rspec
            'Q' -> lift ((P.char '.' >> NominalDiffTime <$> micro) <|> return zero)
                >>= assign tpSecFrac >> go rspec

            -- Year
            'Y' -> fullYear
            'y' -> lift (dec0 2) >>= setCenturyYear
            'C' -> lift (dec0 2) >>= setCentury
            -- Month
            'B' -> lift (indexOfCI $ fst <$> months l) >>= setMonth . succ
            'b' -> lift (indexOfCI $ snd <$> months l) >>= setMonth . succ
            'h' -> lift (indexOfCI $ snd <$> months l) >>= setMonth . succ
            'm' -> lift (dec0 2) >>= setMonth
            -- DayOfMonth
            'd' -> lift (dec0 2) >>= setDayOfMonth
            'e' -> (lift (dec_ 2) >>= setDayOfMonth)
                <|> (lift (dec_ 1) >>= setDayOfMonth)
            -- DayOfYear
            'j' -> lift (dec0 3) >>= assign tpDayOfYear
                >> flag IsOrdinalDate .= True >> go rspec

            -- Year (WeekDate)
            -- FIXME: problematic if input contains both %Y and %G
            'G' -> flag IsWeekDate .= True >> fullYear
            'g' -> flag IsWeekDate .= True >> lift (dec0 2) >>= setCenturyYear
            'f' -> flag IsWeekDate .= True >> lift (dec0 2) >>= setCentury
            -- WeekOfYear
            -- FIXME: problematic if more than one of the following
            'V' -> flag IsWeekDate .= True >> lift (dec0 2) >>= setWeekOfYear
            'U' -> flag IsSundayWeek .= True >> lift (dec0 2) >>= setWeekOfYear
            'W' -> flag IsMondayWeek .= True >> lift (dec0 2) >>= setWeekOfYear
            -- DayOfWeek
            'w' -> lift (dec0 1) >>= setDayOfWeek
            'u' -> lift (dec0 1) >>= setDayOfWeek
            'A' -> lift (indexOfCI $ fst <$> wDays l) >>= setDayOfWeek
            'a' -> lift (indexOfCI $ snd <$> wDays l) >>= setDayOfWeek

            -- TimeZone
            'z' -> do tzOffset; go rspec
            'N' -> do tzOffset; go rspec
            'Z' -> do tzOffset <|> tzName; go rspec
            -- UTCTime
            's' -> do
                s <- lift (negative P.decimal)
                tpPOSIXTime .= fromMicroseconds (1000000 * s)
                flag IsPOSIXTime .= True
                go rspec

            -- modifier (whatever)
            '-' -> go ('%' : rspec)
            '_' -> go ('%' : rspec)
            '0' -> go ('%' : rspec)
            -- escape (why would anyone need %t and %n?)
            '%' -> lift (P.char '%') >> go rspec
            _ -> lift . fail $ "Unknown format character: " ++ show cspec

          where
            l = defaultTimeLocale
            dayHalf = do
                pm <- lift $ False <$ stringCI (fst $ amPm l)
                    <|> True <$ stringCI (snd $ amPm l)
                flag PostMeridiem .= pm
                flag TwelveHour .= True
                go rspec
            -- NOTE: if a greedy parse fails or causes a later failure,
            -- then backtrack and only accept 4-digit years; see #5.
            fullYear = year (negative P.decimal) <|> year (dec0 4)
              where
                year p = do
                    (c, y) <- (`divMod` 100) <$> lift p
                    flag HasCentury .= True
                    tpCentury .= c
                    tpCenturyYear .= y
                    go rspec
            setHour12 h = do
                flag TwelveHour .= True
                tpHour .= h
                go rspec
            setHour24 h = do
                flag TwelveHour .= False
                tpHour .= h
                go rspec
            setCenturyYear y = do tpCenturyYear .= y; go rspec
            setCentury c = do
                tpCentury .= c
                flag HasCentury .= True
                go rspec
            setMonth m = do
                flag IsGregorian .= True
                tpMonth .= m
                go rspec
            setDayOfMonth d = do
                flag IsGregorian .= True
                tpDayOfMonth .= d
                go rspec
            setWeekOfYear w = do tpWeekOfYear .= w; go rspec
            setDayOfWeek d = do tpDayOfWeek .= d; go rspec
            tzOffset = do
                s <- lift (id <$ P.char '+' <|> negate <$ P.char '-')
                h <- lift (dec0 2)
                () <$ lift (P.char ':') <|> pure ()
                m <- lift (dec0 2)
                tpTimeZone . timeZoneMinutes .= s (h * 60 + m)
            tzName = lift timeZoneParser >>= assign tpTimeZone

        c : rspec | P.isSpace c ->
            lift (P.takeWhile P.isSpace) >> go (dropWhile P.isSpace rspec)
        c : rspec | isAscii c -> lift (P.char c) >> go rspec
        c : rspec -> lift (charU8 c) >> go rspec
        "" -> return ()

    micro :: Parser Int64
    micro = do
        us10 <- either fail return . P.parseOnly P.decimal . S.take 7
            . (`S.append` S.pack "000000") =<< P.takeWhile1 P.isDigit
        return (div (us10 + 5) 10)
    {-# INLINE micro #-}

    unixEpoch :: TimeParse
    unixEpoch = TimeParse
        { _tpCentury = 19
        , _tpCenturyYear = 70
        , _tpMonth = 1
        , _tpWeekOfYear = 1
        , _tpDayOfYear = 1
        , _tpDayOfMonth = 1
        , _tpDayOfWeek = 4
        , _tpFlags = 0
        , _tpHour = 0
        , _tpMinute = 0
        , _tpSecond = 0
        , _tpSecFrac = zero
        , _tpPOSIXTime = zero
        , _tpTimeZone = utc
        }
    {-# INLINE unixEpoch #-}
{-# INLINEABLE timeParser #-}

parseTime :: String -> String -> Maybe UTCTime
parseTime spec = either (const Nothing) Just
    . P.parseOnly parser . utf8String
  where
    parser = buildTime <$ P.skipSpace <*> timeParser spec
        <* P.skipSpace <* P.endOfInput
{-# INLINEABLE parseTime #-}

readTime :: (ParseTime t) => String -> String -> t
readTime spec = either error id . P.parseOnly parser . utf8String
  where
    parser = buildTime <$ P.skipSpace <*> timeParser spec
        <* P.skipSpace <* P.endOfInput
{-# INLINEABLE readTime #-}

readsTime :: (ParseTime t) => String -> ReadS t
readsTime spec = parserToReadS $
    buildTime <$ P.skipSpace <*> timeParser spec
{-# INLINEABLE readsTime #-}

-- -------------------------------------------------------------------------- --
-- Build Parse Time

class ParseTime t where
    buildTime :: TimeParse -> t

instance ParseTime TimeOfDay where
    buildTime tp = TimeOfDay h (_tpMinute tp)
        (fromMicroseconds (1000000 * fromIntegral (_tpSecond tp)) ^+^ _tpSecFrac tp)
      where
        h = if tp ^. flag TwelveHour
              then if tp ^. flag PostMeridiem
                then if _tpHour tp < 12
                  then _tpHour tp + 12
                  else _tpHour tp
                else mod (_tpHour tp) 12
              else _tpHour tp
    {-# INLINE buildTime #-}

instance ParseTime YearMonthDay where
    buildTime tp = YearMonthDay (tpYear tp) (_tpMonth tp) (_tpDayOfMonth tp)
    {-# INLINE buildTime #-}

instance ParseTime OrdinalDate where
    buildTime tp = OrdinalDate (tpYear tp) (_tpDayOfYear tp)
    {-# INLINE buildTime #-}

instance ParseTime WeekDate where
    buildTime tp = WeekDate (tpYear tp) (_tpWeekOfYear tp)
        (if _tpDayOfWeek tp == 0 then 7 else _tpDayOfWeek tp)
    {-# INLINE buildTime #-}

instance ParseTime SundayWeek where
    buildTime tp = SundayWeek (tpYear tp) (_tpWeekOfYear tp)
        (if _tpDayOfWeek tp == 7 then 0 else _tpDayOfWeek tp)
    {-# INLINE buildTime #-}

instance ParseTime MondayWeek where
    buildTime tp = MondayWeek (tpYear tp) (_tpWeekOfYear tp)
        (if _tpDayOfWeek tp == 0 then 7 else _tpDayOfWeek tp)
    {-# INLINE buildTime #-}

instance ParseTime ModifiedJulianDay where
    {-# INLINE buildTime #-}
    buildTime tp
        | tp ^. flag IsOrdinalDate = fromOrdinalDate $ buildTime tp
        | tp ^. flag IsGregorian = toGregorian $ buildTime tp
        | tp ^. flag IsWeekDate = fromWeekDate $ buildTime tp
        | tp ^. flag IsSundayWeek = fromSundayWeek $ buildTime tp
        | tp ^. flag IsMondayWeek = fromMondayWeek $ buildTime tp
        | otherwise = fromOrdinalDate $ buildTime tp
        -- TODO: Better conflict handling when multiple flags are set?

instance ParseTime TimeZone where
    buildTime = _tpTimeZone
    {-# INLINE buildTime #-}

instance ParseTime UTCTime where
    buildTime tp = if tp ^. flag IsPOSIXTime
        then  fromPosixTimestampMicros $ toMicroseconds $ _tpPOSIXTime tp
        else zoned
      where
        d :: ModifiedJulianDay
        d = buildTime tp

        dt :: TimeOfDay
        dt = buildTime tp

        tz :: TimeZone
        tz = buildTime tp

        jul :: ModifiedJulianDate
        jul = ModifiedJulianDate d (toDayTime dt)

        zoned :: UTCTime
        zoned = fromModifiedJulianDate jul .+^ timeZoneOffset tz

        toDayTime :: TimeOfDay -> NominalDiffTime
        toDayTime (TimeOfDay h m s) = s
            ^+^ m *^ NominalDiffTime 60000000
            ^+^ h *^ NominalDiffTime 3600000000
        {-# INLINEABLE toDayTime #-}
    {-# INLINE buildTime #-}

-- -------------------------------------------------------------------------- --
-- Time Zone Parser

timeZoneParser :: Parser TimeZone
timeZoneParser = zone "TAI" 0 False <|> zone "UT1" 0 False

    <|> zone "ZULU" (($+) 00 00) False --  Same as UTC
    <|> zone "Z" (($+) 00 00) False --  Same as UTC
    <|> zone "YST" (($-) 09 00) False -- Yukon Standard Time
    <|> zone "YDT" (($-) 08 00) True -- Yukon Daylight-Saving Time
    <|> zone "WST" (($+) 08 00) False -- West Australian Standard Time
    <|> zone "WETDST" (($+) 01 00) True -- Western European Daylight-Saving Time
    <|> zone "WET" (($+) 00 00) False --  Western European Time
    <|> zone "WDT" (($+) 09 00) True -- West Australian Daylight-Saving Time
    <|> zone "WAT" (($-) 01 00) False -- West Africa Time
    <|> zone "WAST" (($+) 07 00) False -- West Australian Standard Time
    <|> zone "WADT" (($+) 08 00) True -- West Australian Daylight-Saving Time
    <|> zone "UTC" (($+) 00 00) False --  Universal Coordinated Time
    <|> zone "UT" (($+) 00 00) False --  Universal Time
    <|> zone "TFT" (($+) 05 00) False -- Kerguelen Time
    <|> zone "SWT" (($+) 01 00) False -- Swedish Winter Time
    <|> zone "SST" (($+) 02 00) False -- Swedish Summer Time
    <|> zone "SET" (($+) 01 00) False -- Seychelles Time
    <|> zone "SCT" (($+) 04 00) False -- Mahe Island Time
    <|> zone "SAST" (($+) 09 30) False -- South Australia Standard Time
    <|> zone "SADT" (($+) 10 30) True -- South Australian Daylight-Saving Time
    <|> zone "RET" (($+) 04 00) False -- Reunion Island Time
    <|> zone "PST" (($-) 08 00) False -- Pacific Standard Time
    <|> zone "PDT" (($-) 07 00) True -- Pacific Daylight-Saving Time
    <|> zone "NZT" (($+) 12 00) False -- New Zealand Time
    <|> zone "NZST" (($+) 12 00) False -- New Zealand Standard Time
    <|> zone "NZDT" (($+) 13 00) True -- New Zealand Daylight-Saving Time
    <|> zone "NT" (($-) 11 00) False -- Nome Time
    <|> zone "NST" (($-) 03 30) False -- Newfoundland Standard Time
    <|> zone "NOR" (($+) 01 00) False -- Norway Standard Time
    <|> zone "NFT" (($-) 03 30) False -- Newfoundland Standard Time
    <|> zone "NDT" (($-) 02 30) True -- Newfoundland Daylight-Saving Time
    <|> zone "MVT" (($+) 05 00) False -- Maldives Island Time
    <|> zone "MUT" (($+) 04 00) False -- Mauritius Island Time
    <|> zone "MT" (($+) 08 30) False -- Moluccas Time
    <|> zone "MST" (($-) 07 00) False -- Mountain Standard Time
    <|> zone "MMT" (($+) 06 30) False -- Myanmar Time
    <|> zone "MHT" (($+) 09 00) False -- Kwajalein Time
    <|> zone "MEZ" (($+) 01 00) False -- Mitteleuropaeische Zeit
    <|> zone "MEWT" (($+) 01 00) False -- Middle European Winter Time
    <|> zone "METDST" (($+) 02 00) True -- Middle Europe Daylight-Saving Time
    <|> zone "MET" (($+) 01 00) False -- Middle European Time
    <|> zone "MEST" (($+) 02 00) False -- Middle European Summer Time
    <|> zone "MDT" (($-) 06 00) True -- Mountain Daylight-Saving Time
    <|> zone "MAWT" (($+) 06 00) False -- Mawson (Antarctica) Time
    <|> zone "MART" (($-) 09 30) False -- Marquesas Time
    <|> zone "LIGT" (($+) 10 00) False -- Melbourne, Australia
    <|> zone "KST" (($+) 09 00) False -- Korea Standard Time
    <|> zone "JT" (($+) 07 30) False -- Java Time
    <|> zone "JST" (($+) 09 00) False -- Japan Standard Time, Russia zone 8
    <|> zone "IT" (($+) 03 30) False -- Iran Time
    <|> zone "IST" (($+) 02 00) False -- Israel Standard Time
    <|> zone "IRT" (($+) 03 30) False -- Iran Time
    <|> zone "IOT" (($+) 05 00) False -- Indian Chagos Time
    <|> zone "IDLW" (($-) 12 00) False -- International Date Line, West
    <|> zone "IDLE" (($+) 12 00) False -- International Date Line, East
    <|> zone "HST" (($-) 10 00) False -- Hawaii Standard Time
    <|> zone "HMT" (($+) 03 00) False -- Hellas Mediterranean Time (?)
    <|> zone "HDT" (($-) 09 00) True -- Hawaii/Alaska Daylight-Saving Time
    <|> zone "GST" (($+) 10 00) False -- Guam Standard Time, Russia zone 9
    <|> zone "GMT" (($+) 00 00) False --  Greenwich Mean Time
    <|> zone "FWT" (($+) 02 00) False -- French Winter Time
    <|> zone "FST" (($+) 01 00) False -- French Summer Time
    <|> zone "FNT" (($-) 02 00) False -- Fernando de Noronha Time
    <|> zone "FNST" (($-) 01 00) False -- Fernando de Noronha Summer Time
    <|> zone "EST" (($-) 05 00) False -- Eastern Standard Time
    <|> zone "EETDST" (($+) 03 00) True -- Eastern Europe Daylight-Saving Time
    <|> zone "EET" (($+) 02 00) False -- Eastern European Time, Russia zone 1
    <|> zone "EDT" (($-) 04 00) True -- Eastern Daylight-Saving Time
    <|> zone "EAT" (($+) 03 00) False -- Antananarivo, Comoro Time
    <|> zone "EAST" (($+) 10 00) False -- East Australian Standard Time
    <|> zone "EAST" (($+) 04 00) False -- Antananarivo Summer Time
    <|> zone "DNT" (($+) 01 00) False -- Dansk Normal Tid
    <|> zone "CXT" (($+) 07 00) False -- Christmas (Island) Time
    <|> zone "CST" (($-) 06 00) False -- Central Standard Time
    <|> zone "CETDST" (($+) 02 00) True -- Central European Daylight-Saving Time
    <|> zone "CET" (($+) 01 00) False -- Central European Time
    <|> zone "CEST" (($+) 02 00) False -- Central European Summer Time
    <|> zone "CDT" (($-) 05 00) True -- Central Daylight-Saving Time
    <|> zone "CCT" (($+) 08 00) False -- China Coastal Time
    <|> zone "CAT" (($-) 10 00) False -- Central Alaska Time
    <|> zone "CAST" (($+) 09 30) False -- Central Australia Standard Time
    <|> zone "CADT" (($+) 10 30) True -- Central Australia Daylight-Saving Time
    <|> zone "BT" (($+) 03 00) False -- Baghdad Time
    <|> zone "BST" (($+) 01 00) False -- British Summer Time
    <|> zone "BRT" (($-) 03 00) False -- Brasilia Time
    <|> zone "BRST" (($-) 02 00) False -- Brasilia Summer Time
    <|> zone "BDST" (($+) 02 00) False -- British Double Summer Time
    <|> zone "AWT" (($-) 03 00) False -- (unknown)
    <|> zone "AWST" (($+) 08 00) False -- Australia Western Standard Time
    <|> zone "AWSST" (($+) 09 00) False -- Australia Western Summer Standard Time
    <|> zone "AST" (($-) 04 00) False -- Atlantic Standard Time (Canada)
    <|> zone "ALMT" (($+) 06 00) False -- Almaty Time
    <|> zone "ALMST" (($+) 07 00) False -- Almaty Summer Time
    <|> zone "AKST" (($-) 09 00) False -- Alaska Standard Time
    <|> zone "AKDT" (($-) 08 00) True -- Alaska Daylight-Saving Time
    <|> zone "AHST" (($-) 10 00) False -- Alaska/Hawaii Standard Time
    <|> zone "AFT" (($+) 04 30) False -- Afghanistan Time
    <|> zone "AEST" (($+) 10 00) False -- Australia Eastern Standard Time
    <|> zone "AESST" (($+) 11 00) False -- Australia Eastern Summer Standard Time
    <|> zone "ADT" (($-) 03 00) True -- Atlantic Daylight-Saving Time
    <|> zone "ACT" (($-) 05 00) False -- Atlantic/Porto Acre Standard Time
    <|> zone "ACST" (($-) 04 00) False -- Atlantic/Porto Acre Summer Time
    <|> zone "ACSST" (($+) 10 30) False -- Central Australia Summer Standard Time
  where
    zone name offset dst = TimeZone offset dst name <$ P.string (S.pack name)
    ($+) h m = h * 60 + m
    ($-) h m = negate (h * 60 + m)

-- -------------------------------------------------------------------------- --
-- Orphan Read Instances

instance Read UTCTime where
    readsPrec _ = readParen False $
        readsTime "%Y-%m-%d %H:%M:%S%Q %Z"
    {-# INLINEABLE readsPrec #-}

-- -------------------------------------------------------------------------- --
-- Orphan Show Instances

instance Show UTCTime where
    showsPrec _ = formatTimeS "%Y-%m-%d %H:%M:%S%Q %Z"

instance Show NominalDiffTime where
    showsPrec p (NominalDiffTime a) rest = showsPrec p a ('s' : rest)

-- -------------------------------------------------------------------------- --
-- Orphan Aeson instances

instance ToJSON UTCTime where
    toJSON t = String $ T.pack $ formatTime "%FT%T%QZ" t
    {-# INLINE toJSON #-}

instance FromJSON UTCTime where
    parseJSON = withText "UTCTime" $ \t ->
        case parseTime "%FT%T%QZ" (T.unpack t) of
          Just d -> pure d
          _      -> fail "could not parse ISO-8601 date"
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- TimeParse Lenses

tpCentury :: Lens' TimeParse Int
tpCentury = lens _tpCentury (\a b -> a { _tpCentury = b })
{-# INLINE tpCentury #-}

tpCenturyYear :: Lens' TimeParse Int
tpCenturyYear = lens _tpCenturyYear (\a b -> a { _tpCenturyYear = b })
{-# INLINE tpCenturyYear #-}

tpMonth :: Lens' TimeParse Int
tpMonth = lens _tpMonth (\a b -> a { _tpMonth = b })
{-# INLINE tpMonth #-}

tpWeekOfYear :: Lens' TimeParse Int
tpWeekOfYear = lens _tpWeekOfYear (\a b -> a { _tpWeekOfYear = b })
{-# INLINE tpWeekOfYear #-}

tpDayOfMonth :: Lens' TimeParse Int
tpDayOfMonth = lens _tpDayOfMonth (\a b -> a { _tpDayOfMonth = b })
{-# INLINE tpDayOfMonth #-}

tpDayOfYear :: Lens' TimeParse Int
tpDayOfYear = lens _tpDayOfYear (\a b -> a { _tpDayOfYear = b })
{-# INLINE tpDayOfYear #-}

tpDayOfWeek :: Lens' TimeParse Int
tpDayOfWeek = lens _tpDayOfWeek (\a b -> a { _tpDayOfWeek = b })
{-# INLINE tpDayOfWeek #-}

tpFlags :: Lens' TimeParse Int
tpFlags = lens _tpFlags (\a b -> a { _tpFlags = b })
{-# INLINE tpFlags #-}

tpHour :: Lens' TimeParse Int
tpHour = lens _tpHour (\a b -> a { _tpHour = b })
{-# INLINE tpHour #-}

tpMinute :: Lens' TimeParse Int
tpMinute = lens _tpMinute (\a b -> a { _tpMinute = b })
{-# INLINE tpMinute #-}

tpSecond :: Lens' TimeParse Int
tpSecond = lens _tpSecond (\a b -> a { _tpSecond = b })
{-# INLINE tpSecond #-}

tpSecFrac :: Lens' TimeParse NominalDiffTime
tpSecFrac = lens _tpSecFrac (\a b -> a { _tpSecFrac = b })
{-# INLINE tpSecFrac #-}

tpPOSIXTime :: Lens' TimeParse NominalDiffTime
tpPOSIXTime = lens _tpPOSIXTime (\a b -> a { _tpPOSIXTime = b })
{-# INLINE tpPOSIXTime #-}

tpTimeZone :: Lens' TimeParse TimeZone
tpTimeZone = lens _tpTimeZone (\a b -> a { _tpTimeZone = b })
{-# INLINE tpTimeZone #-}

