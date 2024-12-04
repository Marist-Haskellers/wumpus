module Main (main) where

import Lib
import Types
import System.Random(mkStdGen)


main :: IO()
main = do
    let gen = mkStdGen 27
    let (ws, newGen) = initializeWumpus gen
    let enviornment = initializeEnvironment newGen
    print ws
    print enviornment

main :: IO ()
main = do
    initialState <- initializeGameState 27
    gameLoop initialState



