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



