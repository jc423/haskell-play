data Position = QB | RB | WR | TE deriving (Show, Eq)

data Player = Player { position :: Position
                     , name :: String
                     } deriving (Show)

positionCount :: [Player] -> Position -> Int
positionCount players pos = length $ filter (\x -> position x == pos) players

team :: Player -> Player -> Player -> Player -> Player -> Player -> Player -> [Player]
team a b c d e f g  | validQbCount && validRBCount && validWRCount = a:b:c:d:e:f:g:[]
                    | otherwise = error "invalid team"
                    where playerList = a:b:c:d:e:f:g:[]
                          validQbCount = positionCount playerList QB == 1
                          validRBCount = positionCount playerList RB == 2
                          validWRCount = positionCount playerList WR == 2
