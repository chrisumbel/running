{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Framework.TH (defaultMainGenerator)

import Test.HUnit
import Test.Framework.Providers.HUnit (testCase)

import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Control.Monad

import qualified Time as T

main = $(defaultMainGenerator)

-- Basic HUnit tests

-- timeToSeconds success
case_timeToSeconds_00_00_00 =
  T.timeToSeconds "0:00:00" @?= Just 0

case_timeToSeconds_12_34_56 =
  T.timeToSeconds "12:34:56" @?= Just 45296

-- timeToSeconds failure
case_timeToSeconds_bad_chars =
  T.timeToSeconds "0:x0:59" @?= Nothing

{- 
-- NOT YET CONVINCED I WANT THESE TO FAIL

-- case_timeToSeconds_single_second_digit =
  T.timeToSeconds "0:00:8" @?= Nothing

-- case_timeToSeconds_seconds_out_of_range =
  T.timeToSeconds "0:00:70" @?= Nothing

-- case_timeToSeconds_single_minute_digit =
  T.timeToSeconds "0:0:59" @?= Nothing

-- case_timeToSeconds_minutes_out_of_range =
  T.timeToSeconds "0:60:59" @?= Nothing
-}

-- secondsToTime
case_secondsToTime_0 =
  T.secondsToTime 0 @?= "00:00:00"

case_secondsToTime_45296 =
  T.secondsToTime 45296 @?= "12:34:56"
