--  File     : Main.hs
--  Author   : Peter Schachte
--  Purpose  : Test program for proj2 project to be used in Grok


-- module Main where
-- import System.CPUTime

-- import System.Exit
-- import Proj2 (Location, toLocation, fromLocation, feedback,
--               GameState, initialGuess, nextGuess)

-- -- testCase = "F1 D2 G4"
-- -- testCase = "H1 B2 D3"
-- -- testCase = "A1 B2 D3"
-- -- testCase = "G1 G2 G3"
-- -- testCase = "C3 H2 H3"
-- -- testCase = "A1 H1 H4"
-- -- testCase = "C2 D2 E2"
-- -- testCase = "A4 D2 H1"
-- -- testCase = "A4 D4 H4"
-- -- testCase = "D2 D3 E3"

-- testCase = "C3 F1 F3"

-- main :: IO ()
-- main = do
--     start <- getCPUTime
--     case mapM toLocation $ words testCase of
--         Just target@[_,_,_] ->
--             proj2test target
--         _ -> do
--             putStrLn $ "toLocation Failed to convert one of " ++ testCase
--                         ++ " to a Location"
--             exitFailure
--     end <- getCPUTime
--     let diff = fromIntegral (end - start) / (10^12)
--     putStrLn $ "Computation time: " ++ show diff ++ " seconds"



-- -- | Guess the given target, counting and showing the guesses.
-- proj2test :: [Location] -> IO ()
-- proj2test target = do
--   putStrLn $ "Searching for target " ++ showLocations target
--   let (guess,other) = initialGuess
--   loop target guess other 1


-- -- | Given a target and guess and a guess number, continue guessing
-- -- until the right target is guessed.
-- loop :: [Location] -> [Location] -> Proj2.GameState -> Int -> IO ()
-- loop target guess other guesses = do
--   putStrLn $ "Your guess #" ++ show guesses ++ ":  " ++ showLocations guess
--   let answer = feedback target guess
--   putStrLn $ "    My answer:  " ++ show answer
--   if answer == (3,0,0)
--     then do
--       putStrLn $ "You got it in " ++ show guesses ++ " guesses!"
--     else do
--       let (guess',other') = nextGuess (guess,other) answer
--       loop target guess' other' (guesses+1)


-- showLocations :: [Location] -> String
-- showLocations = unwords . (fromLocation <$>)


module Main where
import System.CPUTime

import System.Exit
import Proj2 (Location, toLocation, fromLocation, feedback,
              GameState, initialGuess, nextGuess)

main :: IO ()
main = do
    start <- getCPUTime

    -- Generate all test cases
    let allTestCases = [c1:r1:' ':c2:r2:' ':c3:r3:[] | 
                        c1 <- "ABCDEFGH", r1 <- "1234", 
                        c2 <- "ABCDEFGH", r2 <- "1234", 
                        c3 <- "ABCDEFGH", r3 <- "1234", 
                        (c1,r1) < (c2,r2), (c2,r2) < (c3,r3)]
    results <- mapM runTestCase allTestCases

    let totalGuesses = sum results
        avgGuesses = (fromIntegral totalGuesses) / (fromIntegral (length allTestCases))

    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10^12)
    
    putStrLn $ "Average number of guesses for all test cases: " ++ show avgGuesses
    putStrLn $ "Total computation time: " ++ show diff ++ " seconds"

runTestCase :: String -> IO Int
runTestCase testCase = do
    -- putStrLn $ "Running test case: " ++ testCase
    case mapM toLocation $ words testCase of
        Just target@[_,_,_] -> do
            guesses <- proj2test target
            putStrLn $ "Test case " ++ testCase ++ " took " ++ show guesses ++ " guesses."
            return guesses
        _ -> do
            putStrLn $ "toLocation Failed to convert one of " ++ testCase
                        ++ " to a Location"
            exitFailure

-- This function now returns the number of guesses for the test case
proj2test :: [Location] -> IO Int
proj2test target = do
    let (guess,other) = initialGuess
    loop target guess other 1

loop :: [Location] -> [Location] -> GameState -> Int -> IO Int
loop target guess other guesses = do
    let answer = feedback target guess
    if answer == (3,0,0)
        then return guesses
        else do
            let (guess',other') = nextGuess (guess,other) answer
            loop target guess' other' (guesses+1)

showLocations :: [Location] -> String
showLocations = unwords . (fromLocation <$>)
