data Position = QB | RB | WR | TE deriving (Show, Eq)

data Player = Player { position :: Position
                     , name :: String
                     } deriving (Show)

team :: Player -> Player -> [Player]
team x y | position x /= position y = [x, y]
         | otherwise = [x]
