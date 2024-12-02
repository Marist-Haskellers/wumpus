module Lib
    ( startGame
    ) where
import Types (GameState(..), Choice(..), PlayerState(..))
import IO (getChoiceFromUser, getMoveFromUser)
import GameLogic (StartGameState (..), createStartState, decahedron)
import System.Random

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
    choice <- getChoiceFromUser
    case choice of
        ChoiceMove -> do
            move <- getMoveFromUser
            let curPos = currentPosition (playerState gameState)
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
            -- Handle Sense logic
            return gameState
        ChoiceShoot -> do
            -- Handle Shoot logic
            return gameState



