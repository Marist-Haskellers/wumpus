module Lib where

import Types
import System.Random
import Data.List ( nub )
import Control.Monad.Cont (when)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

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
                Back -> lastPos
                Types.Left -> getNextPosition connections lastPos
                Types.Right -> getPrevPosition connections lastPos
    where
        getNextPosition :: [Position] -> Position -> Position
        getNextPosition conns lastP =
            case dropWhile (/= lastP) conns of
                (_:next:_) -> next  
                _ -> head conns     

        getPrevPosition :: [Position] -> Position -> Position
        getPrevPosition conns lastP =
            case dropWhile (/= lastP) (reverse conns) of
                (_:next:_) -> next  
                _ -> last conns     

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

checkSense :: Int -> IO ()
    

gameLoop :: GameState -> IO ()
gameLoop gs = do
    -- print current state
    print $ "You are in cave " ++ show (playerPosition $ player gs)
    
    -- check win/lose conditions
    when (checkWumpus gs) $ do
        putStrLn "You were eaten by the Wumpus! Game Over."
        return ()

    case checkHazards gs of
        Just Pit -> do
            putStrLn "You fell into a pit! Game Over."
            return ()
            
        Just Bats -> do
            putStrLn "Bats carried you away!" 
            -- BAT TELEPORT NEEDED

        Nothing -> do
            checkSense (playerPosition $ player gs)
            -- get player move
            putStrLn "Choose your move (L/R/B):"
            moveStr <- getLine
            let move = case moveStr of
                    "L" -> Types.Left
                    "R" -> Types.Right
                    "B" -> Types.Back
                    _ -> error "Invalid move"
            
            -- process move and continue
            gameLoop $ processMove gs move