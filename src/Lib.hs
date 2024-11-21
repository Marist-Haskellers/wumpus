module Lib
    ( someFunc,
    initializeWumpus,
    initializeEnvironment
    ) where

import Types ( WumpusState(WumpusState), wumpusPosition, Position, Hazard (..), PlayerState (..), lastPostion, playerPosition)
import System.Random ( StdGen, randomR, randomRs )
import Data.List ( nub )

someFunc :: IO ()
someFunc = putStrLn "someFunc"

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

