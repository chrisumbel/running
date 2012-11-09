
module Time (
  timeToSeconds,
  secondsToTime
) where

-- Time to Seconds
parseDigit :: [Char] -> [Char] -> Char -> ([Char], [Char])
parseDigit "" digits current = ("", ([current] ++ digits))
parseDigit time digits ':' = (time, digits)
parseDigit time digits current = 
  parseDigit (init time) ([current] ++ digits) (last time)


parseSegment :: ([Char], [Char]) -> ([Char], Int)
parseSegment (time, segment) = (time, (read segment))

-- TODO: return Nothing on parse fail
parseSegment' :: [Char] -> Maybe ([Char], Int)
parseSegment' time = Just (parseSegment (parseDigit (init time) "" (last time)))

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

-- Seconds to Time
lead :: Int -> [Char]
lead n
  | n < 10     = "0"
  | otherwise  = ""

show' :: Int -> [Char]
show' n = (lead n) ++ (show n)

generateSegment :: Int -> Int -> [Char]
generateSegment seconds depth
  | depth == 0 = (generateSegment seconds 1) ++ (show' (seconds `mod` 60))
  | depth  < 3 = (generateSegment seconds (depth + 1)) ++ 
    show' ((seconds `div` (60 ^ depth)) `mod` (60 ^ depth)) ++ ":"
  | otherwise = ""

secondsToTime :: Int -> [Char]
secondsToTime seconds = 
  generateSegment seconds 0 
