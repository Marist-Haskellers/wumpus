module Main (main) where

import Lib
import Types
import System.Random(mkStdGen)

main :: IO ()
main = do
    initialState <- initializeGameState 27
    gameLoop initialState



