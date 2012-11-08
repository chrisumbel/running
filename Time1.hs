{-|
  Time to string conversions
-}
module Time1 where

{-|
  Convert time string to a time in seconds.
-}
timeToSeconds :: String    -- ^ should be in format ...HHHH:MM:SS
              -> Maybe Int -- ^ time in seconds, if successful parse
timeToSeconds = undefined
-- TODO

{-|
  Convert time in seconds to string format
-}
secondsToTime :: Int    -- ^ time in seconds >= 0
              -> String -- ^ ...HHHH:MM:SS
secondsToTime = undefined
-- TODO
