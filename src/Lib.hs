module Lib
    ( startGame
    ) where
import Types (GameState(..), Choice(..), PlayerState(..))
import IO (getChoiceFromUser, getMoveFromUser, output, getSenseFromUser)
import GameLogic (StartGameState (..), createStartState, decahedron)
import System.Random
import Hazards

startGame :: IO GameState
startGame = do 
    let startState = createStartState StartGameState {
        startRandomGen=mkStdGen 42,
        playerLastPostion=2,
        playerCurrentPosition=1,
        playerArrowCount=5,
        numberOfPits=2,
        numberOfBats=2,
        caveLayout=decahedron
    }
    oneLoop startState

oneLoop :: GameState -> IO GameState
oneLoop gameState = do
    let curPos = currentPosition (playerState gameState)
    output ("You are currently in room " ++ show curPos)
    choice <- getChoiceFromUser
    case choice of
        ChoiceMove -> do
            move <- getMoveFromUser
            let oldPos = lastPosition (playerState gameState)
            let newPos = mover gameState curPos oldPos move
            oneLoop GameState {
                mover=mover gameState,
                wumpusState=wumpusState gameState,
                randomGen=randomGen gameState,
                playerState= PlayerState {
                    lastPosition=curPos,
                    currentPosition=newPos,
                    arrowCount=arrowCount (playerState gameState)
                    },
                environmentState=environmentState gameState
                }
        ChoiceSense -> do
            sense <- getSenseFromUser
            let sensed = senseHazards gameState
            let senseString = if sense `elem` sensed then "You sense: " else "You do not sense: "
            output (senseString ++ show sense)
            oneLoop gameState
        ChoiceShoot -> do
            -- Handle Shoot logic
            return gameState


-- -- Function to collect up to 5 moves from the user
-- shootArrow :: IO [Move]
-- shootArrow = collectMoves 5 []  -- Start with an empty list and a max count of 5

-- -- Helper function to recursively collect moves
-- collectMoves :: Int -> [Move] -> IO [Move]
-- collectMoves 0 moves = return moves  -- Stop when the maximum number of moves is reached
-- collectMoves remaining moves = do
--     putStrLn $ "Moves collected so far: " ++ show moves
--     putStrLn $ "You can input up to " ++ show remaining ++ " more moves."
--     move <- getArrowMoveFromUser
--     case move of 
--         Nothing -> return moves -- return the list smaller than 5
--         Just validMove -> collectMoves (remaining - 1) (moves ++ [validMove])  -- Append the move and decrement the counter
