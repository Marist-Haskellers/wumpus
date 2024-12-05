{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (module Main) where

import Control.Monad (unless, when)
import Data.Char (toLower)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Lib
import System.Random (StdGen, newStdGen)
import Types

main :: IO ()
main = do
  putStrLn "Welcome to Hunt the Wumpus!"
  rules
  putStrLn "To see the rules at any time just type 'rules'."
  putStrLn ""
  gen <- newStdGen
  let initialGameState = initialState gen -- Initialize game state
  debug False initialGameState -- Debugging for game state
  gameLoopIO initialGameState -- Begin game loop

gameLoopIO :: GameState -> IO ()
gameLoopIO state = do
  let caveData = "\nYou're inside cave #" ++ showState state -- Display current cave
  case gameStatus state of
    GameOver reason -> do
      putStrLn caveData
      putStrLn $ "Game over! Reason: " ++ reason
    Ongoing message -> do
      unless (null message) (putStrLn $ "\n" ++ message) -- Print the ongoing status message if present
      putStrLn caveData
      putStrLn "Perform an action:"
      input <- getLine
      if map toLower input == "rules"
        then do
          rules
          gameLoopIO state -- Continue the game loop after displaying the rules
        else case parseInput state input of
          Just action -> do
            let newState = setState (playerState state) action state
            gameLoopIO newState
          Nothing -> do
            putStrLn "\nInvalid action"
            gameLoopIO state -- Re-ask for input

-- Helper function to check if an action is a sense
isSenseAction :: Action -> Bool
isSenseAction (SenseAction _) = True
isSenseAction _ = False

debug :: Bool -> GameState -> IO ()
debug debugging state =
  when debugging $ do
    putStrLn "Debugging Enabled: Initial Game State"
    putStrLn $ "Player Position: " ++ show (playerPosition $ playerState state)
    putStrLn $ "Player Last Position: " ++ show (lastPosition $ playerState state)
    putStrLn $ "Player Arrow Count: " ++ show (playerArrowCount $ playerState state)
    putStrLn $ "Wumpus Position: " ++ show (wumpusPosition $ wumpusState state)
    putStrLn $ "Hazards: " ++ show (hazards $ environmentState state)
    putStrLn $ "Game Status: " ++ show (gameStatus state)
    putStrLn ""

parseInput :: GameState -> String -> Maybe Action
parseInput state input =
  -- break the input into lowercase words to be further distinguished
  case words (map toLower input) of
    ("move" : dir : _) -> case dir of
      "left" -> Just $ MoveAction MoveLeft
      "right" -> Just $ MoveAction MoveRight
      "back" -> Just $ MoveAction MoveBack
      _ -> Nothing
    ("shoot" : path) ->
      let currentPos = playerPosition $ playerState state
          parsedPath = translatePath currentPos path decahedron
       in if all isJust parsedPath -- if all paths are just the parse path, and not nothing
            then Just $ ShootAction (map fromJust parsedPath) -- then shoot with path whose just part is removed from the parsed paths
            else Nothing
    ("sense" : sense : _) -> case sense of
      "smell" -> Just $ SenseAction SmellWumpus
      "listen" -> Just $ SenseAction HearBats
      "feel" -> Just $ SenseAction FeelDraft
      _ -> Nothing
    _ -> Nothing

-- Helper function to parse the arrows path into a route through the caves
translatePath :: Position -> [String] -> CaveLayout -> [Maybe Position]
translatePath _ [] _ = [] -- base case
translatePath currentPos (dir : dirs) layout =
  -- using the current position and layout
  case lookup currentPos layout of
    -- if there is a path instruction
    Just connections ->
      -- next position corresponds to where the arrow will end up
      let nextPos = case dir of
            "left" -> Just (head connections) -- Left cave
            "right" -> Just (connections !! 1) -- Right cave
            "back" -> Just (connections !! 2) -- Previous cave
            _ -> Nothing
       in nextPos : translatePath (fromMaybe currentPos nextPos) dirs layout
    -- translate this position into a direction for handleShooting in Lib.hs
    Nothing -> [Nothing]

-- Show room #
showState :: GameState -> String
showState state = show (playerPosition $ playerState state)

-- Initial game state
initialState :: StdGen -> GameState
initialState gen =
  let -- Generate random positions for hazards using `getRandomPosition` from Lib.hs
      (bats1, gen1) = findSafePosition gen [] decahedron
      (bats2, gen2) = findSafePosition gen1 [(bats1, Bats)] decahedron
      (pits1, gen3) = findSafePosition gen2 [(bats1, Bats), (bats2, Bats)] decahedron
      (pits2, gen4) = findSafePosition gen3 [(bats1, Bats), (bats2, Bats), (pits1, Pit)] decahedron
      hazards = [(bats1, Bats), (bats2, Bats), (pits1, Pit), (pits2, Pit)]

      -- Generate Wumpus position
      (wumpusPos, gen5) = findSafePosition gen4 hazards decahedron

      -- Ensure the player starts in a safe position
      (playerPos, lastPos, gen6) = findSafePlayerPosition gen5 hazards decahedron

      -- Create initial states
      player = Player {playerPosition = playerPos, lastPosition = lastPos, playerArrowCount = 3, playerHasShot = False}
      wumpus = WumpusState {wumpusPosition = wumpusPos}
      environment = EnvironmentState {hazards = hazards}
   in GameState {playerState = player, wumpusState = wumpus, environmentState = environment, gen = gen6, gameStatus = Ongoing ""}

rules :: IO ()
rules = do
  putStrLn ""
  putStrLn "-- Rules --"
  putStrLn ""
  putStrLn "Welcome to Hunt the Wumpus! Your mission is to locate and kill the deadly Wumpus hiding in the caves."
  putStrLn "Kill the Wumpus by shooting it with an arrow. Be careful, the Wumpus may kill you or flee if you startle it!"
  putStrLn "Be careful not to enter the Wumpus's cave, its putrid smell is too potent and instantly kills you."
  putStrLn ""
  putStrLn "2. How to play:"
  putStrLn "   - Move <direction>: Move to a connected cave. Choose either 'move left', 'move right', or 'move back'."
  putStrLn "   - Sense <sense>: Use your senses to gather information on your surroundings."
  putStrLn "       'sense smell' - Detect if the Wumpus is nearby."
  putStrLn "       'sense listen' - Listen for the flapping wings of bats."
  putStrLn "       'sense feel' - Feel for drafts near a bottomless pit."
  putStrLn "   - Shoot <path>: Fire an arrow on a path of up to 5 caves. The arrow may hit or scare the Wumpus."
  putStrLn "       'shoot left right back...' - Detect if the Wumpus is nearby."
  putStrLn ""
  putStrLn "3. Hazards in the Caves:"
  putStrLn "   - The Wumpus: If you enter its cave, there's a 50/50 chance it will either kill you or flee to a neighboring cave."
  putStrLn "   - Bottomless Pits: Falling into one results in an instant game over!"
  putStrLn "   - Cave Bats: These creatures will transport you to a random cave, possibly putting you in harm's way."
  putStrLn ""
  putStrLn "4. Winning the Game:"
  putStrLn "   - Kill the Wumpus by shooting it with an arrow. Beware: you only have 3 arrows!"
  putStrLn "   - Use your senses and logic to deduce the Wumpus's location and hazards in nearby caves."
  putStrLn ""
  putStrLn "5. Strategy Tips:"
  putStrLn "   - Use your senses wisely to gather information before moving or shooting."
  putStrLn "   - Plan your moves carefully to avoid hazards."
  putStrLn "   - If you miss the Wumpus, it may flee and change its position, so stay alert!"
  putStrLn ""
  putStrLn "6. Important Notes:"
  putStrLn "   - The Wumpus does not move unless startled by an arrow."
  putStrLn "   - Arrows cannot travel more than 5 caves in a single shot."
  putStrLn ""
  putStrLn "Good luck, brave adventurer! May your aim be true and your steps cautious."
  putStrLn ""
