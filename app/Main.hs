module Main (main) where

import Lib
import Types
import System.Random(mkStdGen)

<<<<<<< HEAD
main :: IO()
main = do
    let gen = mkStdGen 27
    let (ws, newGen) = initializeWumpus gen
    let enviornment = initializeEnvironment newGen
    print ws
    print enviornment
=======
main :: IO ()
main = do
    initialState <- initializeGameState 27
    gameLoop initialState
>>>>>>> 744ce6c9946fd13de3fa566765eca47bca5a2c40



