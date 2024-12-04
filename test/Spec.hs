module Spec (module Spec) where

import Types

testState :: GameState
testState =
  let player = Player {playerPosition = 1, lastPosition = 2, playerArrowCount = 3, playerHasShot = False}
      wumpus = WumpusState {wumpusPosition = 7}
      environment =
        EnvironmentState
          { hazards =
              [ (3, Bats),
                (5, Pit),
                (8, Bats),
                (12, Pit)
              ]
          }
   in GameState
        { playerState = player,
          wumpusState = wumpus,
          environmentState = environment,
          gen = undefined, -- Not required for set tests
          gameStatus = Ongoing
        }
