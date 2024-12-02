## Determining all directions for all possible connections 

### Old Move Code
This didn't work in practice since it would work for some and not for others. However, there is a trend where some caves have their left and right shifted and the `mod 3` indexes would need to be reversed. I can't find a pattern to faciliate this though.
```
testCaveLayout :: CaveLayout
testCaveLayout =
  [ (1,  [2, 5, 8]),
    (2,  [1, 3, 10]),
    (3,  [2, 4, 12]),
    (4,  [3, 5, 14]),
    (5,  [1, 4, 6]),
    (6,  [5, 7, 15]),
    (7,  [6, 8, 17]),
    (8,  [1, 7, 9]),
    (9,  [8, 10, 18]),
    (10, [2, 9, 11]),
    (11, [10, 12, 19]),
    (12, [3, 11, 13]),
    (13, [12, 14, 20]),
    (14, [4, 13, 15]),
    (15, [6, 14, 16]),
    (16, [15, 17, 20]),
    (17, [7, 16, 18]),
    (18, [9, 17, 19]),
    (19, [11, 18, 20]),
    (20, [13, 16, 19])
  ]

-- Moves the player, in main this would be called before setState
-- CaveLayout, currentPosition, previousPosition, noveDirection, new position
move :: CaveLayout -> Position -> Position -> Move -> Position
move layout currentPosition previousPosition moveType =
  case lookup currentPosition layout of
    Just connections ->
      case elemIndex previousPosition connections of
        Just backIndex ->
          let leftIndex  = (backIndex + 1) `mod` 3 -- Clockwise (Left)
              rightIndex = (backIndex - 1 + 3) `mod` 3 -- Counterclockwise (Right)
              back       = previousPosition
              left       = connections !! leftIndex
              right      = connections !! rightIndex
          in case moveType of
               MoveBack  -> back
               MoveLeft  -> left
               MoveRight -> right
        Nothing -> error $ "Previous position " ++ show previousPosition ++ " not found in connections for " ++ show currentPosition
    Nothing -> error $ "Invalid current position: " ++ show currentPosition
```
Assuming we're working in a clockwise order, I've noticed a pattern where the **right** cave always comes before the **behind** cave and the **left** comes after it. So we can get it left via `(index + 1) mod 3` and right via `(index - 1 + 3) mod 3`.
```
1, [_,5,8] {B,R,L}

1, [2,_,8] {L,B,R}

1, [2,5,_] {R,L,B}

2, [_,3,10] {B,L,R}

2, [1,_,10] {R,B,L}

2, [1,3,_] {L,R,B}

3, [_,4,12] {B,L,R}

3, [2,_,12] {R,B,L}

3, [2,4,_] {L,R,B}

4, [_,5,14] {B,L,R}

4, [3,_,14] {R,B,L}

4, [3,5,_] {L,R,B}

5, [_,4,6] {B,R,L}

5, [1,_,6] {L,B,R}

5, [1,4,_] {R,L,B}

6, [_,7,15] {B,L,R}

6, [5,_,15] {R,B,L}

6, [5,7,_] {L,R,B}

7, [_,8,17] {B,L,R}

7, [6,_,17] {R,B,L}

7, [6,8,_] {L,R,B}

8, [_,7,9] {B,R,L}

8, [1,_,9] {L,B,R}

8, [1,7,_] {R,L,B}

9, [_,10,18] {B,L,R}

9, [8,_,18] {R,B,L}

9, [8,10,_] {L,R,B}

10, [_,9,11] {B,R,L}

10, [2,_,11] {L,B,R}

10, [2,9,_] {R,L,B}

11, [_,12,19] {B,L,R}

11, [10,_,19] {R,B,L}

11, [10,12,_] {L,R,B}

12, [_,11,13] {B,R,L}

12, [3,_,13] {L,B,R}

12, [3,11,_] {R,L,B}

13, [_,14,20] {B,L,R}

13, [12,_,20] {R,B,L}

13, [12,14,_] {L,R,B}

14, [_,13,15] {B,R,L}

14, [4,_,15] {L,B,R}

14, [4,13,_] {R,L,B}

15, [_,14,16] {B,R,L}

15, [6,_,16] {L,B,R}

15, [6,14,_] {R,L,B}

16, [_,17,20] {B,L,R}

16, [15,_,20] {R,B,L}

16, [15,17,_] {L,R,B}

17, [_,16,18] {B,R,L}

17, [7,_,18] {L,B,R}

17, [7,16,_] {R,L,B}

18, [_,17,19] {B,R,L}

18, [9,_,19] {L,B,R}

18, [9,17,_] {R,L,B}

19, [_,18,20] {B,R,L}

19, [11,_,20] {L,B,R}

19, [11,18,_] {R,L,B}

20, [_,16,19] {B,L,R}

20, [13,_,19] {R,B,L} 

20, [13,16,_] {L,R,B}
```