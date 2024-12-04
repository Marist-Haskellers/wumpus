module Main (main) where

import Control.Monad.State
import Data.Bool (not)
import Functions
import Lib
import System.Exit (exitFailure)
import System.IO
import System.Random (randomRIO)
import Types

main :: IO ()
main = do
  hSetEncoding stdout utf8

  -- Define the initial game state
  putStrLn caveArt
  let caveRooms = [1 .. 20]
  environment <- randomizeEnvironment caveRooms

  let initialState =
        GameState
          { playerState = Player 1 2 5, -- Position 1, Previous is 2, 3 Arrows
            environment = environment,
            layout = decahedron
          }

  gameLoop initialState

gameLoop :: GameState -> IO ()
gameLoop game = do
  -- DOING THIS HERE (BEFORE OTHER LOOP) SO THAT I CAN PROVIDE INFORMATION TO THE MENU
  let player@(Player c p a) = playerState game
      adjustedNeighbors = getOrientationAdjustedNeighbors c p (layout game)

  {- UNCOMMENT TO DEBUG
  let pitPositions = pitsLocations $ environment game
  putStrLn $ "[DEBUG] Pits Positions: " ++ show pitPositions

  let batPositions = batsLocations $ environment game
  putStrLn $ "[DEBUG] Bats Positions: " ++ show batPositions

  let wumpusPosition = wumpusLocation $ environment game
  putStrLn $ "[DEBUG] Wumpus Positions: " ++ show wumpusPosition

  let player@(Player c p a) = playerState game
      adjustedNeighbors = getOrientationAdjustedNeighbors c p (layout game)
  putStrLn $ "[DEBUG] Orientation Adjusted Neighbors: " ++ show adjustedNeighbors
  -}

  putStrLn $ headsUpDisplay (adjustedNeighbors) -- remember why we need the `$` here
  putStrLn menuHeader
  putStrLn $ formatPlayerState (playerState game)
  putStrLn $ menuBodyImproved (adjustedNeighbors) -- remember why we need the `$` here
  input <- getLine
  let parsedInput = case input of
        "Left" -> Just $ Movement Types.Left
        "Right" -> Just $ Movement Types.Right
        "Back" -> Just $ Movement Types.Back
        "Smell" -> Just $ Action "Smell"
        "Listen" -> Just $ Action "Listen"
        "Feel" -> Just $ Action "Feel"
        "Shoot Left" -> Just $ Action "Shoot Left"
        "Shoot Right" -> Just $ Action "Shoot Right"
        _ -> Nothing -- error "Invalid move"
        -- call gameLoop again, but now with the updated state
  case parsedInput of
    Just (Movement move) -> do
      updatedGame <- execStateT (movePlayer move) game
      gameLoop updatedGame
    Just (Action "Smell") -> do
      putStrLn $ smell game
      gameLoop game
    Just (Action "Listen") -> do
      putStrLn $ listen game
      gameLoop game
    Just (Action "Feel") -> do
      putStrLn $ feel game
      gameLoop game
    Just (Action "Shoot Left") -> do
      -- let result = shoot "Left" game
      result <- evalStateT (shoot "Left") game -- above was pure function implementation, but that doesn't work since it operates in our StateT GameState IO monad
      putStrLn result
      if result == "You killed the Wumpus! You win!"
        then putStrLn "Game Over. Thanks for playing."
        else gameLoop game
    Just (Action "Shoot Right") -> do
      result <- evalStateT (shoot "Right") game
      putStrLn result
      if result == "You killed the Wumpus! You win!"
        then putStrLn "Game Over. Thanks for playing."
        else gameLoop game
    Just (Action action) -> do
      putStrLn $ "Unknown action: " ++ action
      gameLoop game
    Nothing -> do
      putStrLn "Invalid input. Try again."
      gameLoop game

movePlayer :: Move -> StateT GameState IO ()
movePlayer direction = do
  game <- get -- access current game state
  let player@(Player current prev arrows) = playerState game
      {-
        `playerState game` retrieves/extracts playerState from the GameState, `game`

        On the left side within the parenthesis, we have (Player current prev arrows)
          What this is doing is pattern-matching the structure of what is returned on the right side of the assignment
          `Player` deconstructs playerState into its individual components;
              current (playerPosition),
              prev (lastPosition),
              arrows (playerArrowCount)

        The `player@(...)` syntax binds the entire value contained within the parenthesis to the variable `player`
        This is not copying, or creating a new variable in the sense of duplicating memory,
          This is a `name binding` which refers to the entire value of `playerState game`
          `player` refers back directly to the original value
      -}
      gameLayout = layout game -- resolves issue with reuse of `layout`
      neighbors = getNeighbors current gameLayout
      adjustedNeighbors = getOrientationAdjustedNeighbors current prev gameLayout
      newPosition = case direction of
        Types.Back -> adjustedNeighbors !! 0
        Types.Left -> adjustedNeighbors !! 2
        Types.Right -> adjustedNeighbors !! 1
      env = environment game
      isWumpus = newPosition == wumpusLocation env
      -- wumpusWouldAttack         = randomRIO (False, True)
      -- wumpusAttacked            = isWumpus && wumpusWouldAttack
      -- wumpusRanAway             = isWumpus && not wumpusAttacked
      isBat = newPosition `elem` batsLocations env -- If players new pos is same as cave with bats
      isPit = newPosition `elem` pitsLocations env -- If players new pos is same as cave with pit
      -- updatedPlayer = Player newPosition current arrows
  wumpusWouldAttack <- liftIO $ randomRIO (False, True)
  let wumpusAttacked = isWumpus && wumpusWouldAttack
      wumpusRanAway = isWumpus && not wumpusAttacked

  when wumpusAttacked $ liftIO $ do
    putStrLn "You encountered the Wumpus and it attacked! GG no re"
    exitFailure

  when wumpusRanAway $ do
    liftIO $ putStrLn "WOAH! That was the WUMPUS! It ran away!"
    newWumpusPosition <- liftIO $ randomRIO (1, 20)
    let updatedEnvironment = env {wumpusLocation = newWumpusPosition}
    put $ game {environment = updatedEnvironment}

  when isBat $ liftIO $ putStrLn "Bats! You have now been carried to a random cave."

  when isPit $ liftIO $ do
    putStrLn "You fell into a pit and died."
    exitFailure

  {-
  when wumpusAttacked  $ liftIO $ putStrLn "You encountered the Wumpus and it attacked! GG no re"
  when wumpusRanAway  $ liftIO $ putStrLn "WOAH! That was the WUMPUS! It ran away!"
  when isPit          $ liftIO $ putStrLn "You fell into a pit and died."
  when isBat          $ liftIO $ putStrLn "Bats! You have now been carried to a random cave."
  -}

  finalPosition <-
    if isBat
      then liftIO $ randomRIO (1, 20)
      else return newPosition
  let newPrevPosition =
        if isBat
          then head [back | (pos, back : _) <- decahedron, pos == finalPosition]
          else current
      updatedPlayer = Player finalPosition newPrevPosition arrows
  {-
    This should fix issue where when a player gets dropped in a new cave,
    the previous position was still corresponded to their location
    prior to being flown away and placed in a random cave.

    List comprehension explained:
    It takes the dodecahedron and matches pos with the new final position
    it then extracts the list of neighbors via `back:_`
    and then it returns the `back` which was found

    UPDATE: It did fix the issue.
  -}

  put $ game {playerState = updatedPlayer}
