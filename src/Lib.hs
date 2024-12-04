{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Lib (module Lib) where

import Data.Maybe (fromMaybe)
import System.Random as Random
import Types

-- Move function that is called during setState and handleMovement
move :: MoveLayout -> Position -> Position -> Move -> Position
move layout currentPosition previousPosition moveType =
  case lookup (currentPosition, previousPosition) layout of
    Just [left, right] ->
      case moveType of
        MoveBack -> previousPosition -- Move back to the previous position
        MoveLeft -> left -- Move to the left cave
        MoveRight -> right -- Move to the right cave
    Just _ ->
      error $
        "Invalid layout entry for currentPosition: "
          ++ show currentPosition
          ++ ", previousPosition: "
          ++ show previousPosition
    Nothing ->
      error $
        "No valid move found for currentPosition: "
          ++ show currentPosition
          ++ ", previousPosition: "
          ++ show previousPosition

-- Function that modifies the state of the game depending on the players actions
setState :: PlayerState -> Action -> GameState -> GameState
setState player action gameState =
  case action of
    MoveAction moveDir -> handleMovement player moveDir gameState
    ShootAction path -> handleShooting player path gameState
    SenseAction sense -> handleSensing player sense gameState

-- Function to handle movement of player
-- Take in current player state, action they performed, current game state, and gives new game state
handleMovement :: PlayerState -> Move -> GameState -> GameState
handleMovement player moveDir gameState =
  let -- Extract fields from GameState
      wumpus = wumpusState gameState
      env = environmentState gameState
      genVal = gen gameState
      -- status = gameStatus gameState     This will be used once main and game is working

      currentPos = playerPosition player
      lastPos = lastPosition player

      hazardsList = hazards env
      wumpusPos = wumpusPosition wumpus

      -- Determine new position based on moveDir
      newPos = move caveMap currentPos lastPos moveDir

      -- Update PlayerState
      updatedPlayer =
        player
          { lastPosition = currentPos,
            playerPosition = newPos,
            playerHasShot = False -- Reset shot status every time the player moves
          }

      -- Check for hazards at the new position
      hazard = lookup newPos hazardsList

      -- Check if player has encountered the Wumpus
      wumpusEncounter = newPos == wumpusPos

      -- Handle hazard effects
      (interimPlayer, interimGen, interimStatus) = case hazard of
        Just Bats ->
          -- Transport player to a random position
          let (randPos, randLastPos, newStdGen) = getRandomPosition genVal decahedron
              transportedPlayer =
                updatedPlayer
                  { playerPosition = randPos,
                    lastPosition = randLastPos
                  }
           in (transportedPlayer, newStdGen, Ongoing "The bats grab you by the arms and fly you to an unknown cave.")
        Just Pit ->
          -- Player falls into a pit, game over
          (updatedPlayer, genVal, GameOver "You fell into a pit!")
        Nothing ->
          -- No hazard encountered
          (updatedPlayer, genVal, Ongoing "")

      -- Handle Wumpus encounter with random chance
      (finalPlayer, finalWumpus, finalGen, finalStatus) =
        if wumpusEncounter
          then
            let (chance, newStdGen) = Random.randomR (1, 2) interimGen :: (Int, Random.StdGen) -- 1: Player dies, 2: Wumpus flees
             in if chance == 1
                  then
                    -- Player dies
                    (interimPlayer, wumpus, newStdGen, GameOver "You were eaten by the Wumpus!")
                  else
                    -- Wumpus flees to a random adjacent cave
                    let connections = fromMaybe [] (lookup wumpusPos decahedron)
                        (idx, updatedGen') = Random.randomR (0, length connections - 1) newStdGen :: (Int, Random.StdGen)
                        newWumpusPos = connections !! idx
                        newWumpus = wumpus {wumpusPosition = newWumpusPos}
                     in (interimPlayer, newWumpus, updatedGen', Ongoing "The wumpus flees to a random cave.")
          else
            -- No Wumpus encounter
            (interimPlayer, wumpus, interimGen, interimStatus)

      -- Determine the final game status
      finalGameStatus = finalStatus
   in -- Return the updated GameState
      gameState
        { playerState = finalPlayer,
          wumpusState = finalWumpus,
          gen = finalGen,
          gameStatus = finalGameStatus
        }

handleShooting :: PlayerState -> [Position] -> GameState -> GameState
handleShooting player path gameState =
  let genVal = gen gameState
      wumpus = wumpusState gameState
      wumpusPos = wumpusPosition wumpus
      remainingArrows = playerArrowCount player - 1

      -- Check if the player has arrows left
      gameStatus' =
        if remainingArrows < 0
          then GameOver "You have no arrows left!"
          else gameStatus gameState

      -- Check if the arrow path exceeds the maximum length
      gameStatus'' =
        if length path > 5
          then GameOver "Your arrow cannot travel more than 5 caves!"
          else gameStatus'

      -- Validate the arrow path
      isValidPath = validateArrowPath (playerPosition player) path

      -- Update game status if path is invalid
      gameStatus''' =
        if not isValidPath
          then GameOver "Invalid arrow path!"
          else gameStatus''

      -- Determine if the arrow hits the Wumpus
      arrowHitsWumpus = wumpusPos `elem` path

      -- Update game status if the Wumpus is hit
      finalGameStatus =
        if arrowHitsWumpus
          then GameOver "You have killed the Wumpus! You win!"
          else gameStatus'''

      -- If the arrow misses and the game is ongoing, the Wumpus may move
      (updatedWumpusState, finalGen) =
        if not arrowHitsWumpus && finalGameStatus == Ongoing "Your arrow missed the Wumpus."
          then moveWumpusRandomly wumpus genVal
          else (wumpus, genVal)

      -- Update the player's arrow count
      updatedPlayer =
        player
          { playerArrowCount = remainingArrows,
            playerHasShot = True
          }

      statusMessage =
        if arrowHitsWumpus
          then "Your arrow hit the Wumpus!"
          else "Your arrow missed the Wumpus. You have " ++ show remainingArrows ++ " arrows left."
   in gameState
        { playerState = updatedPlayer,
          wumpusState = updatedWumpusState,
          gen = finalGen,
          gameStatus = Ongoing statusMessage
        }

handleSensing :: PlayerState -> Sense -> GameState -> GameState
handleSensing player sense gameState =
  let currentPos = playerPosition player
      senseData = generateSenseData decahedron gameState
      message = case lookup currentPos senseData of
        Just senseList ->
          let filteredList = filter (== sense) senseList
           in if null filteredList
                then "There are no hazards nearby able to be sensed with this sense."
                else unlines $ map describeSense filteredList
        Nothing -> "You sense nothing unusual."
   in gameState {gameStatus = Ongoing message}

validateArrowPath :: Position -> [Position] -> Bool
validateArrowPath _ [] = True -- Empty path is valid
validateArrowPath currentPos (nextPos : rest) =
  case lookup currentPos decahedron of
    Just connections ->
      (nextPos `elem` connections) && validateArrowPath nextPos rest
    Nothing -> False -- Current position not found in the cave layout

-- Helper function that generates a random position, used for Bats and Wumpus moving
selectRandomElement :: Random.StdGen -> [a] -> (a, Random.StdGen)
selectRandomElement genValue list =
  let (idx, newStdGen) = Random.randomR (0, length list - 1) genValue
      element = list !! idx
   in (element, newStdGen)

-- Used for bats transporting player to a random cave, returns random position and a random last position connected to this position
getRandomPosition :: Random.StdGen -> CaveLayout -> (Position, Position, Random.StdGen)
getRandomPosition genValue cave =
  let allPositions = map fst cave
      (newPos, gen1) = selectRandomElement genValue allPositions -- Random position
      connections = case lookup newPos cave of
        Just conns -> conns
        Nothing -> error $ "Invalid position in cave layout: " ++ show newPos
      (lastPos, gen2) = selectRandomElement gen1 connections -- Random last position from connections
   in (newPos, lastPos, gen2)

moveWumpusRandomly :: WumpusState -> Random.StdGen -> (WumpusState, Random.StdGen)
moveWumpusRandomly currentWumpusState genValue =
  let wumpusPos = wumpusPosition currentWumpusState
      connections = fromMaybe [] (lookup wumpusPos decahedron)
      (newWumpusPos, newGen) = selectRandomElement genValue connections
      updatedWumpus = currentWumpusState {wumpusPosition = newWumpusPos}
   in (updatedWumpus, newGen)

-- I/O helper function to check if a cave has a hazard in it, for spawning the player with a random state
findSafePlayerPosition :: StdGen -> [(Position, Hazard)] -> CaveLayout -> (Position, Position, StdGen)
findSafePlayerPosition gen hazards cave =
  let unsafePositions = map fst hazards
      (playerPos, lastPos, newGen) = getRandomPosition gen cave
   in if playerPos `elem` unsafePositions
        then findSafePlayerPosition newGen hazards cave -- Retry until safe
        else (playerPos, lastPos, newGen)

-- Helper to find a safe position
getStartingPosition :: StdGen -> CaveLayout -> (Position, StdGen)
getStartingPosition gen caveLayout =
  let allPositions = map fst caveLayout
      (newPos, newGen) = selectRandomElement gen allPositions
   in (newPos, newGen)

-- I/O helper function to check if a cave has a hazard in it, for spawning the hazards in a ransom state
findSafePosition :: StdGen -> [(Position, Hazard)] -> CaveLayout -> (Position, StdGen)
findSafePosition gen hazards cave =
  let unsafePositions = map fst hazards
      (newPos, newGen) = getStartingPosition gen cave
   in if newPos `elem` unsafePositions
        then findSafePosition newGen hazards cave -- Retry until safe
        else (newPos, newGen)
