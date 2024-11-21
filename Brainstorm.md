# Brainstorming and Planning

## Data Respresentation

### What data must we represent?

- player
    - where is the player?
    - how many arrows?
    - currently active sense?

- cave
    - how can tell rooms apart? (unique identifier?)
    - attribute/contents (empty/normal, pit, bats, wumpus)
    - should we store the "sense result" as data with each cave?

## Functions and Classes

### What operations/transformations do we need?

- outcome/result of a threat (e.g., bats, pit, wumpus)
- three senses
    - should we compute the result of a sense from other data?

<!-- --------------------------------------------------------------------------- -->


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
