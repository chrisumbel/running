
module Time (
  timeToSeconds,
  secondsToTime
) where

{------------------------------------------------------------------------------
  Time to Seconds
  ----------------------------------------------------------------------------}

{-
  recursively extract a digit out of a time string and append it to a
  segment's string for later conversion
-}  
parseDigits :: [Char] -> [Char] -> Char -> Maybe ([Char], [Char])
parseDigits "" digits current = Just ("", ([current] ++ digits))
parseDigits time digits ':' = Just (time, digits)
parseDigits time digits current 
  | current `elem` ['0'..'9'] = parseDigits (init time) ([current] ++ digits) (last time)
  | otherwise = Nothing

{-
  destructure parseDigits result and convert the current segment's
  string of digits to an Int
-}
parseSegment :: ([Char], [Char]) -> ([Char], Int)
parseSegment (time, segment) = (time, (read segment))

{-
  parse one segment of the users time string out returning the rest
  of the time string and the current segment as an Int i.e.
  "12:34:56" -> Just ("12:34", 56)
-}
parseSegment' :: [Char] -> Maybe ([Char], Int)
parseSegment' time = 
  case parseDigits (init time) "" (last time) of 
    Just (time, segment) -> Just (parseSegment (time, segment))
    otherwise -> Nothing

{-
  determine how many seconds a singe unit in a given section of time
  represents. i.e. if it's the minutes section that's being parsed 
  then a unit represents 60 seconds
-}
depthMultiplier :: Int -> Int
depthMultiplier depth
  | depth == 0  = 1
  | depth == 3  = 24
  | depth  > 3  = 0   -- we don't support it, wipe the result out 
  | otherwise	= 60

{-
  add up the number of seconds in each segment of time i.e. in "12:34"
  the "34" is Just 34
  the "12" is 12 * 60 = 720
  34 + 720 = 754
-}
timeSegmentsToSeconds :: ([Char], Int) -> Int -> Maybe Int
timeSegmentsToSeconds ("", seconds) depth = Just seconds
timeSegmentsToSeconds (time, seconds) depth =
  let segmentTime = parseSegment' time
  in case segmentTime of
      Just segmentTime ->
        case timeSegmentsToSeconds segmentTime (depth + 1) of
          Just n -> Just (seconds + n * (depthMultiplier depth))
          Nothing -> Nothing
      Nothing -> Nothing

{-
  API Entry point

  determine the number of seconds from a time string i.e. "12:34"
  returns Just 754
-}
timeToSeconds :: [Char] -> Maybe Int
timeToSeconds time = timeSegmentsToSeconds (time, 0) 0 

{------------------------------------------------------------------------------
  Seconds to Time
  ----------------------------------------------------------------------------}

{-
  format a segment of time with leading zeros if necessary
-}
formatSegment :: Int -> [Char]
formatSegment n
  | n < 10     = '0' : show n
  | otherwise  = show n

{-
  recusively create a formatted string of time segments
-}
generateSegments :: Int -> Int -> [Char]
generateSegments seconds depth
  | depth == 0 = (generateSegments seconds 1) ++ (formatSegment (seconds `mod` 60))
  | depth  < 3 = (generateSegments seconds (depth + 1)) ++ 
    formatSegment ((seconds `div` (60 ^ depth)) `mod` (60 ^ depth)) ++ ":"
  | otherwise = ""

{-
  API Entry Point

  convert an Int number of seconds to a formatted time string
-}
secondsToTime :: Int -> [Char]
secondsToTime seconds = 
  generateSegments seconds 0 
