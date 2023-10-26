--  File     : Proj2.hs
--  Author   : Yitong Kong
--  ID       : 1254947
--  Purpose  : Guess logic for the game Battleship, the player can use 
--             initialGuess, then nextGuess, nextGuess, ...... to find the three 
--             target ships.

-- | File level documentation: on a 4 by 8 map, there exists 3 target ships, our 
-- aim is to guess the exact locations of them, to achieve this, we first take 
-- an initialGuess, the system will always give us a feedback, inidicating the 
-- number of ships exactly located; the number of guesses that were exactly one 
-- space away from a ship; and the number of guesses that were exactly two 
-- spaces away from a ship. After that, we keep taking nextGuess according to 
-- the last feedback and finally guess all three targets right.

module Proj2 (Location, toLocation, fromLocation, feedback,
              GameState, initialGuess, nextGuess) where

import Data.List ( delete, group, sortOn ) 

-- | Represents the rows in a grid, with values ranging from R1 to R4.
data Row = R1 | R2 | R3 | R4 deriving (Show, Enum, Eq)

-- | Represents the columns in a grid, with values ranging from A to H.
data Column = A | B | C | D | E | F | G | H deriving (Show, Enum, Eq)

-- | Represents a specific location on the grid defined by a column and a row.
data Location = Location Column Row deriving (Show, Eq)

-- | Represents the game state as a list of guesses, a guess is a list which 
-- contains three distinct locations.
type GameState = [[Location]]

-- | Converts a string representation of a grid location to a Location datatype. 
-- If the conversion is not possible, returns Nothing.
toLocation :: String -> Maybe Location
toLocation [c,r] = do
    col <- charToColumn c
    row <- charToRow r
    return (Location col row)
toLocation _ = Nothing

-- | Converts a Location datatype back to a string.
fromLocation :: Location -> String
fromLocation (Location c r) = columnToChar c : [rowToChar r]

-- | Converts a character representation of a row ('1' to '4') to a Row datatype
-- If the character doesn't represent a valid row, returns Nothing.
charToRow :: Char -> Maybe Row
charToRow '1' = Just R1
charToRow '2' = Just R2
charToRow '3' = Just R3
charToRow '4' = Just R4
charToRow _   = Nothing

-- | Converts a character representation of a column ('A' to 'H') to a Column 
-- datatype. If the character doesn't represent a valid column, returns Nothing.
charToColumn :: Char -> Maybe Column
charToColumn 'A' = Just A
charToColumn 'B' = Just B
charToColumn 'C' = Just C
charToColumn 'D' = Just D
charToColumn 'E' = Just E
charToColumn 'F' = Just F
charToColumn 'G' = Just G
charToColumn 'H' = Just H
charToColumn _   = Nothing

-- | Converts a Row datatype to its character representation ('1' to '4').
rowToChar :: Row -> Char
rowToChar R1 = '1'
rowToChar R2 = '2'
rowToChar R3 = '3'
rowToChar R4 = '4'

-- | Converts a Column datatype to its character representation ('A' to 'H').
columnToChar :: Column -> Char
columnToChar A = 'A'
columnToChar B = 'B'
columnToChar C = 'C'
columnToChar D = 'D'
columnToChar E = 'E'
columnToChar F = 'F'
columnToChar G = 'G'
columnToChar H = 'H'

-- | The feedback function computes how many locations in a guess are at which
-- distances from the targets. It returns a tuple of counts for distances 0,1,2.
feedback :: [Location] -> [Location] -> (Int, Int, Int)
feedback targets guesses = convertToFeedback distanceList
  where distanceList = calculateMinDistance targets guesses

-- | The convertToFeedback function takes a list of Int and returns a tuple
-- indicating how many locations are at distances 0, 1, and 2 away from a ship.
convertToFeedback :: [Int] -> (Int, Int, Int)
convertToFeedback xs = (count 0, count 1, count 2)
  where count n = length (filter (== n) xs)

-- | For each location in a guess, computes the distance to all target locations 
-- and keep the minimum one, return as an Int list after finishing computing all 
-- three locations in the guess.
calculateMinDistance :: [Location] -> [Location] -> [Int]
calculateMinDistance targets = map minDistanceToTargets
  where
    minDistanceToTargets location 
        = minimum (map (calculateDistance location) targets)

-- | The calculateDistance function computes the distance between two locations.
-- The distance between two locations is defined as the maximum of the 
-- differences in their row and column coordinates, because the diagonally 
-- adjacent location are counted as distance 1 away.
calculateDistance :: Location -> Location -> Int
calculateDistance (Location c1 r1) (Location c2 r2)
    = max rowDistance colDistance
      where
        rowDistance = abs (fromEnum r2 - fromEnum r1)
        colDistance = abs (fromEnum c2 - fromEnum c1)

-- | After comparation, I choose "A2 D3 H2" as the first guess.
firstGuess :: [Location]
firstGuess = [Location A R2, Location D R3, Location H R2]

-- | The initialGuess function returns the first guess to be used in the game,
-- and the gameState containing other 4959 possible guesses after removing the 
-- first guess.
initialGuess :: ([Location], GameState)
initialGuess = (firstGuess, gameState)
  where gameState = delete firstGuess (combinations 3 allLocations)

-- | allLocations generates a list of all 32 possible locations on the map.
allLocations :: [Location]
allLocations = [Location col row | col <- [A .. H], row <- [R1 .. R4]]

-- | The combinations function generates all possible combinations of a given 
-- size from a list. In our case, this function is used to generate all 4960 
-- possible guesses as there are 4960 ways to choose 3 distinct locations from 
-- 32 locations.
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs

-- | The nextGuess function gives the nextGuess and nextGameState based on the 
-- previous guess, previous gameState, and the feedback received for the 
-- previous guess.
nextGuess :: ([Location], GameState) -> (Int,Int,Int) -> ([Location], GameState)
nextGuess (prevGuess, gameState) prevFeedback = (nextGuess, nextGameState)
  -- | Computing the feedback of each possibleGuess in gameState against 
  -- prevGuess, if not consistent with the prevFeedback, then that guess must 
  -- not be the target, only store with consistent feedback in possibleTargets. 
  where possibleTargets 
          = filter (\g -> feedback g prevGuess == prevFeedback) gameState        
        nextGuess 
          -- | After removing all impossible guesses, if the number of elements 
          -- in possibleTargets is greater than 700, we just choose the guess in 
          -- the middle as the next guess, because calculating the expected 
          -- remaining targets is too time-consuming. However, if the 
          -- possibleTargets has less than 700 elements, we will calculate the 
          -- expected remaining targets and pick the one with the least score as 
          -- our next guess to minimise the total guesses.
          | length possibleTargets > 700 
              = possibleTargets !! (length possibleTargets `div` 2)
          -- | This expression below retrieves the guess with the smallest 
          -- expected remaining candidates from the list of tuples which 
          -- contains the guess along with its expected score.
          | otherwise = fst (head (sortOn snd (compute possibleTargets)))
        nextGameState = delete nextGuess possibleTargets

-- | The compute function calculates the expected score for each location and 
-- attaches the score to the corresponding location, then return.
compute :: [[Location]] -> [([Location], Double)]
compute locations = [(location, scoreFor location) | location <- locations]
  where
    -- | Calculates the expected score for a given location.
    scoreFor location = calculateExpectedScore (feedbacksFor location)
    -- | Retrieves a list of feedback for a given location when compared to all 
    -- other locations.
    feedbacksFor location = map (feedback location) locations

-- | Calculates the expected score for a given list of feedbacks.
-- The function works by:
-- 1. Grouping identical feedbacks together.
-- 2. Squaring the length of each group.
-- 3. Summing up all the squared lengths.
-- 4. Dividing by the total number of feedbacks to compute the score.
-- the score is actually the expected remaining candidates, the less the better.
calculateExpectedScore :: [(Int, Int, Int)] -> Double
calculateExpectedScore feedbacks =
  fromIntegral (sum (map ((^2).length) (group feedbacks))) / 
  fromIntegral (length feedbacks)

