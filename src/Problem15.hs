module Problem15 (result) where

data Direction = D | R deriving (Enum,Show)

solve limit = move 0 0
  where
    canMoveRight x y = x < limit
    canMoveDown x y = y < limit

    possibleDirections x y
      | canMoveRight' && canMoveDown' = [D,R]
      | canMoveRight' && (not canMoveDown') = [R]
      | (not canMoveRight') && canMoveDown' = [D]
      | otherwise = []
      where
        canMoveDown' = canMoveDown x y
        canMoveRight' = canMoveRight x y

    move x y = go possible
      where
        possible = possibleDirections x y
        go [D,R] = move (x+1) y + move x (y+1)
        go [R]   = move (x+1) y
        go [D]   = move x (y+1)
        go []    = 1

result = [solve x | x <- [0..10]]
