module Lib
    ( startGame
    ) where
import Types (GameState(..), Choice(..), PlayerState(..))
import IO (getChoiceFromUser, getMoveFromUser, output)
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
            let hazards = hazards (environmentState gameState)
            let layout = caveLayout (environmentState gameState)
            let sensed = senseHazards curPos layout hazards
            output ("You sense: " ++ unwords (map show sensed))
            oneLoop gameState
            return gameState
        ChoiceShoot -> do
            -- Handle Shoot logic
            return gameState



