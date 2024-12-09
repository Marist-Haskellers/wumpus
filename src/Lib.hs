module Lib where
    -- ( startGame
    -- ) where
import Types (GameState(..), Choice(..), PlayerState(..), WumpusState(..))
import IO (getChoiceFromUser, getMoveFromUser, output, getSenseFromUser, playerMoveToGame)
import GameLogic (StartGameState (..), createStartState, decahedron, shootArrow, updateArrowCount, arrowPathResult, arrowInRoomWithWumpus)
import System.Random
import Hazards

startState :: StartGameState
startState = StartGameState {
        startRandomGen=mkStdGen 42,
        playerLastPostion=0,
        playerCurrentPosition=1,
        playerArrowCount=5,
        numberOfPits=2,
        numberOfBats=2,
        caveLayout=decahedron
    }
startGame :: IO GameState
startGame = do 
    oneLoop (createStartState startState)

oneLoop :: GameState -> IO GameState
oneLoop gameState = do
    let curPos = currentPosition (playerState gameState)
    output ("You are currently in room " ++ show curPos)
    let wumpusPos = wumpusPosition (wumpusState gameState)
    output ("The Wumpus is currently in room " ++ show wumpusPos) -- Debugging line

    choice <- getChoiceFromUser
    case choice of
        ChoiceMove -> do
            move <- getMoveFromUser
            let oldPos = lastPosition (playerState gameState)
            let newPos = mover gameState curPos oldPos move
            oneLoop gameState {
                playerState= PlayerState {
                    lastPosition=curPos,
                    currentPosition=newPos,
                    arrowCount=arrowCount (playerState gameState)
                    }
                }
        ChoiceSense -> do
            sense <- getSenseFromUser
            let sensed = senseHazards gameState
            let senseString = if sense `elem` sensed then "You sense: " else "You do not sense: "
            output (senseString ++ show sense)
            oneLoop gameState
        ChoiceShoot -> do
           if arrowCount (playerState gameState) <= 0
            then do 
                output "No arrows left to shoot."
                oneLoop gameState
            else do 
                output ("Current arrow count: " ++ show (arrowCount (playerState gameState)))
                arrowMoves <- shootArrow
                let startPos = currentPosition (playerState gameState)
                let finalArrowPos = arrowPathResult gameState startPos arrowMoves
                output ("The arrow traveled through the rooms and ended up in room " ++ show finalArrowPos)

                let (message, updatedGameState) =
                        if finalArrowPos == wumpusPosition (wumpusState gameState)
                        then arrowInRoomWithWumpus gameState
                        else ("The arrow missed the Wumpus!", gameState)

                output message

                -- update the arrow count
                let newGameState = updateArrowCount updatedGameState
                oneLoop newGameState


          





