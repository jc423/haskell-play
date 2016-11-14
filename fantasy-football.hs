import qualified Data.Map as Map

data Position = QB | RB | WR | TE deriving (Show, Eq, Ord)

data Player = Player { position :: Position
                     , name :: String
                     } deriving (Show)

type PositionCountMap = Map.Map Position Int

positionCount :: [Player] -> Position -> Int
positionCount players pos = length $ filter (\x -> position x == pos) players

getCountsForPositions :: [Player] -> PositionCountMap
getCountsForPositions playerList = Map.fromList (map (\x -> (x, positionCount playerList x)) [QB, RB, WR, TE])


team :: [Player] -> [Player]
team xs  | validQbCount && validRBCount && validWRCount && validTECount && length xs == 7 = xs
         | otherwise = error "invalid team"
         where positionCounts = getCountsForPositions xs
               validQbCount = Map.lookup QB positionCounts == Just 1
               validRBCount = Map.lookup RB positionCounts >= Just 2
               validWRCount = Map.lookup WR positionCounts >= Just 2
               validTECount = Map.lookup TE positionCounts >= Just 1

qb = Player{position=QB, name="joe"}
rb = Player{position=RB, name="thurman"}
rb2 = Player{position=RB, name="marshawn"}
wr = Player{position=WR, name="andre"}
wr2 = Player{position=WR, name="lee"}
te = Player{position=TE, name="scott"}
