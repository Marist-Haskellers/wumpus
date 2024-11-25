module Main (main) where

import Lib

main :: IO ()
main = someFunc

-- put the player at starting position 
-- randomly assign the pit, bats, and wumpus positions

-- GameLoop plan
    -- getChoiceFromUser 
        -- if move 
            -- getMoveFromUser IO
            -- move logic
            -- update the gameState
                -- if player falls in pit
                    -- break and output loss
                -- if player goes into room and wumpus eats them
                    -- break and output loss
            -- go back to get choice from user
        -- if sense
            -- getSense IO
            -- sense logic (call output to print)
            -- goes back to choice 
            -- can call indefinetely (either different senses)
        -- if shoot
            -- check to make sure they have arrows 
                -- if yes 
                    -- get arrow move IO
                    -- move arrow 
                    -- repeat 5 times CHANGE 
                    -- if wumpus killed 
                        -- break gameLoop and call victory
                    -- else 
                        -- output arrow did not hit 
                        -- go back to playerChoice (start of gameloop)
                -- else 
                    -- output that they cannot do anything 
                    -- go back to playerChoice
    -- break if    
        -- wumpus is killed by arrow (victory)
        -- player falls into pit (loss)
        -- wumpus eats the player (loss)
    

-- IO to see if wumpus was hit (call output from arrow shooting function)
-- IO to see if player falls in pit (call output if player stumbled into a pit )
-- IO to see if wumpus eats player (call output if player eats wumpus)


-- can you use multiple senses in the same room
-- is the arrow shot always 5? 