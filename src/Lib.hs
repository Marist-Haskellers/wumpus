<<<<<<< HEAD
module Lib
    ( someFunc,
    initializeWumpus,
    initializeEnvironment
    ) where
=======
module Lib where

import Types

import System.Random
<<<<<<< HEAD
import Data.List
import Control.Monad.Cont
=======
import Data.List ( nub )
<<<<<<< HEAD
import Control.Monad.Cont (when)
=======
import Data.Char (toLower)
>>>>>>> 268ffa67c9937e4ed115a1f90be7d9c44e211b73
>>>>>>> 744ce6c9946fd13de3fa566765eca47bca5a2c40

import Types ( WumpusState(WumpusState), wumpusPosition, Position, Hazard (..), PlayerState (..), lastPostion, playerPosition)
import System.Random ( StdGen, randomR, randomRs )
import Data.List ( nub )
>>>>>>> f93093e662f14f95989ca8a92cb69de3a9fcc6c9

someFunc :: IO ()
someFunc = putStrLn "someFunc"

<<<<<<< HEAD
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

=======
data Move = MoveLeft | MoveRight | MoveBack deriving Show

initializeWumpus :: StdGen -> (WumpusState, StdGen)
initializeWumpus gen =
    let (pos, newGen) = randomR (1, 20) gen --generates new position
    in (WumpusState { wumpusPosition = pos }, newGen)

--Enviornment initialization
initializeEnvironment :: StdGen -> [(Position, Hazard)]
initializeEnvironment g =
    let positions = take 4 $ nub $ map (+1) $ randomRs (1, 20) g -- unique positions from 1 to 20
        [p1, p2, b1, b2] = positions -- extract positions for hazards
    in [(p1, Pit), (p2, Pit), (b1, Bats), (b2, Bats)]

initializePlayer :: Int -> PlayerState
initializePlayer _ =
    let arrows = 3
        pos = 1
        lastPos = 2 in
        PlayerState {playerPosition = pos, lastPosition = lastPos, playerArrowCount = arrows}

initializeGameState :: Int -> IO GameState
initializeGameState seed = do
    let gen = mkStdGen seed
    let (wumpusState, newGen) = initializeWumpus gen
    let envHazards = initializeEnvironment newGen
    let playerState = initializePlayer 1
    return GameState {
        wumpus = wumpusState,
        environment = EnvironmentState { hazards = envHazards },
        player = playerState
    }

checkHazards :: GameState -> Maybe Hazard
checkHazards gs = lookup (playerPosition $ player gs) (hazards $ environment gs)

checkWumpus :: GameState -> Bool
checkWumpus gs = playerPosition (player gs) == wumpusPosition (wumpus gs)

getPossibleMoves :: Position -> Maybe [Position]
getPossibleMoves pos = lookup pos decahedron

move :: CaveLayout -> Position -> Position -> Move -> Position
move layout currentPos lastPos moveType =
    case lookup currentPos layout of
        Nothing -> error "Invalid position"
        Just connections -> 
            case moveType of
                MoveBack -> lastPos
                MoveLeft -> getLeftMove connections lastPos
                MoveRight -> getRightMove connections lastPos
    where
        getLeftMove :: [Position] -> Position -> Position
        getLeftMove (x:y:z:_) prev
            | prev == x = y
            | prev == y = z
            | prev == z = x
            | otherwise = x
        getLeftMove _ _ = error "Invalid move"

        getRightMove :: [Position] -> Position -> Position
        getRightMove (x:y:z:_) prev
            | prev == x = z
            | prev == y = x
            | prev == z = y
            | otherwise = z
        getRightMove _ _ = error "Invalid move"

processMove :: GameState -> Move -> GameState
processMove gs moveType =
    let currentPos = playerPosition $ player gs
        lastPos = lastPosition $ player gs
        newPos = Lib.move decahedron currentPos lastPos moveType
        newPlayer = (player gs) {
            playerPosition = newPos,
            lastPosition = currentPos
        }
    in gs { player = newPlayer }

checkSense :: GameState -> Position -> IO ()
checkSense gs pos = do
    case lookup pos decahedron of
        Nothing -> return ()
        Just neighbors -> do
            let wumpusNearby = any (\p -> p == wumpusPosition (wumpus gs)) neighbors
            let pitsNearby = any (\p -> any (\(pos, haz) -> pos == p && haz == Pit) (hazards $ environment gs)) neighbors
            let batsNearby = any (\p -> any (\(pos, haz) -> pos == p && haz == Bats) (hazards $ environment gs)) neighbors
            
            when wumpusNearby $ 
                putStrLn "You smell something terrible!"
            when pitsNearby $ 
                putStrLn "You feel a draft!"
            when batsNearby $ 
                putStrLn "You hear rustling!"

gameLoop :: GameState -> IO ()
gameLoop gs = do
    -- Debug print for hazards
    putStrLn "\nDEBUG - Hazard Locations:"
    mapM_ (\(pos, haz) -> putStrLn $ "Hazard at cave " ++ show pos ++ ": " ++ show haz) 
          (hazards $ environment gs)
    putStrLn $ "Wumpus at cave " ++ show (wumpusPosition $ wumpus gs) ++ "\n"
    
    let currentPos = playerPosition $ player gs
    let connections = maybe [] id $ getPossibleMoves currentPos
    
    -- Check death conditions first
    if checkWumpus gs 
    then do
        putStrLn $ "You are in cave " ++ show currentPos
        putStrLn "You were eaten by the Wumpus! Game Over."
        return ()
    else case checkHazards gs of
        Just Pit -> do
            putStrLn $ "You are in cave " ++ show currentPos
            putStrLn "You fell into a pit! Game Over."
            return ()
        Just Bats -> do
            putStrLn $ "You are in cave " ++ show currentPos
            putStrLn "Bats carried you away!"
            -- TODO: Implement bat teleport
            gameLoop $ processMove gs MoveBack  -- Temporary: just move back instead of teleporting
        Nothing -> do
            putStrLn $ "You are in cave " ++ show currentPos
            putStrLn $ "You can go to caves: " ++ formatConnections connections

<<<<<<< HEAD
            -- Print senses
            let nearbyHazards = [(p, h) | p <- connections, (pos, h) <- hazards $ environment gs, p == pos]
            let wumpusNear = any (\p -> p == wumpusPosition (wumpus gs)) connections

            mapM_ (\(_, h) -> case h of 
                Bats -> putStrLn "You hear the fluttering of many wings near by"
                Pit -> putStrLn "You feel a draft nearby") nearbyHazards
            when wumpusNear $ putStrLn "You smell something terrible nearby"

            putStrLn "What would you like to do? (S/L/R/B)"
            putStrLn "S = Shoot Arrow, L = Move Left, R = Move Right, B = Move Back"

            action <- getLine
            case upperCase action of
                "S" -> undefined  -- TODO: Implement shooting
                "L" -> gameLoop $ processMove gs MoveLeft
                "R" -> gameLoop $ processMove gs MoveRight
                "B" -> gameLoop $ processMove gs MoveBack
                _ -> do
                    putStrLn "Invalid action"
                    gameLoop gs
    where
        upperCase = map (\c -> if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c)

-- helper formatting functions
formatConnections :: [Position] -> String
formatConnections [] = ""
formatConnections [x] = show x
formatConnections [x,y] = show x ++ " or " ++ show y
formatConnections [x,y,z] = show x ++ ", " ++ show z ++ " or " ++ show y 
formatConnections (x:xs) = show x ++ ", " ++ formatConnections xs
=======
>>>>>>> 268ffa67c9937e4ed115a1f90be7d9c44e211b73
>>>>>>> 744ce6c9946fd13de3fa566765eca47bca5a2c40
>>>>>>> f93093e662f14f95989ca8a92cb69de3a9fcc6c9
