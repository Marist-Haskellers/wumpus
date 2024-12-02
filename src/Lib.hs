module Lib
    ( someFunc,
    initializeWumpus,
    initializeEnvironment,
    initializePlayer
    ) where

import Types
import System.Random ( StdGen, randomR, randomRs )
import Data.List ( nub )
import Data.Char (toLower)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Move = MoveLeft | MoveRight | MoveBack deriving Show

initializeWumpus :: StdGen -> (WumpusState, StdGen)
initializeWumpus gen =
    let (pos, newGen) = randomR (1, 20) gen --generates new position
    in (WumpusState { wumpusPosition = pos }, newGen)

--Enviornment initialization
initializeEnvironment :: StdGen -> [(Position, Hazard)]
initializeEnvironment g =
    let positions = take 4 $ nub $ map (+1) $ randomRs (1, 20) g -- Unique positions from 1 to 20
        [p1, p2, b1, b2] = positions -- Extract positions for hazards
    in [(p1, Pit), (p2, Pit), (b1, Bats), (b2, Bats)]


initializePlayer :: Int -> PlayerState
initializePlayer _ =
    let arrows = 3
        pos = 1
        lastPos = 2 in
        PlayerState {playerPosition = pos, lastPosition = lastPos, playerArrowCount = arrows}

-- [back, left, right]
parseInput :: IO Move
parseInput = do
    putStrLn "Enter a Move (Left, Right, Back): "
    input <- getLine
    case map toLower input of
        "left" -> return MoveLeft
        "right" -> return MoveRight
        "back" -> return MoveBack

