module Lib where
    -- ( startGame
    -- ) where
import Types (GameState(..), Choice(..), PlayerState(..))
import IO (getChoiceFromUser, getMoveFromUser, output, getSenseFromUser)
import GameLogic (StartGameState (..), createStartState, decahedron, shootArrow)
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
    -- output (map . show (hazards (environmentState gameState)))
    let curPos = currentPosition (playerState gameState)
    output ("You are currently in room " ++ show curPos)
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
            -- check gameState to see if you have arrows in the first place (0 will give a specific prompt)
            arrowEnumList <- shootArrow -- collect the arrow moves

            -- the arrow will have to move through the rooms, and on the last one will have to see if it hit the wumpus or if it startled the wumpus
            -- also update the arrow count
            oneLoop gameState





