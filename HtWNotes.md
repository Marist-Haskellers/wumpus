
## What data must we represent?
# character:
   position
   action:
      senses:
         touch
         hearing
         smell
      fire arrow:
         5 arrows (how many do we have?)
         num of rooms traveled (max 5)
      move location:
         3 directions (dif numbers)
         1 room

# locations:
   [1..20] eNum -> num = loc
   def n:
      location:
         3 neighbor nums
         enemy/nothing
   bottomless pit

# enemy:
   wumpus:
      smells
   bats:
      flapping
   hole:
      windy

## What operations/transformations will we need?
# character interactions:
      sense:
         choose sense to use:
            hear
            smell
            touch
      fire arrow:
         choose arrow distance
      movment:
         1 room per turn
         3 directional choices

# enemy interactions:
   wumpus (smells when in neighboring room):
      sleeping in random room
      when arrow is fired:
         kills wumpus
         startles wumpus
   bats (flaps):

   hole (windy):

# location interactions:
   movement:
      3 directions (dif numbers)
