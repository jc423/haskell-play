import qualified Data.Map as Map

data Position = QB | RB | WR | TE | DEF | K deriving (Show, Eq, Ord)

data Player = Player { position :: Position
                     , name :: String
                     , proj :: Int
                     }

sumPlayer :: [Player] -> Int
sumPlayer [] = 0
sumPlayer (x:xs) = proj x + sumPlayer xs

instance Show Player where
  show p = name p

type PositionCountMap = Map.Map Position Int

positionCount :: [Player] -> Position -> Int
positionCount players pos = length $ filter (\x -> position x == pos) players

getCountsForPositions :: [Player] -> PositionCountMap
getCountsForPositions playerList = Map.fromList (map (\x -> (x, positionCount playerList x)) [QB, RB, WR, TE, DEF, K])


validTeam :: [Player] -> Bool
validTeam xs  = validQbCount && validRBCount && validWRCount
  && validTECount && validDEFCount && validKCount && length xs == 9
  where positionCounts = getCountsForPositions xs
        validQbCount = Map.lookup QB positionCounts == Just 1
        validRBCount = Map.lookup RB positionCounts >= Just 2
        validWRCount = Map.lookup WR positionCounts >= Just 2
        validTECount = Map.lookup TE positionCounts >= Just 1
        validDEFCount = Map.lookup DEF positionCounts == Just 1
        validKCount = Map.lookup K positionCounts == Just 1

teamCombos :: Int -> [Player] -> [[Player]]
teamCombos n p = combinations n p 


combinations 0 _ = [[]]
combinations _ [] = []
combinations n xs@(y:ys)
 | n < 0     = []
 | otherwise = case drop (n-1) xs of
                 [ ] -> []
                 [_] -> [xs]
                 _   -> [y:c | c <- combinations (n-1) ys]
                           ++ combinations n ys

qb = Player{position=QB, name="joe", proj=20}
qb2 = Player{position=QB, name="jim", proj=10}
rb = Player{position=RB, name="thurman", proj=15}
rb2 = Player{position=RB, name="marshawn", proj=12}
rb3 = Player{position=RB, name="freddie", proj=10}
rb4 = Player{position=RB, name="lasean", proj=9}
wr = Player{position=WR, name="andre", proj=18}
wr2 = Player{position=WR, name="lee", proj=17}
wr3 = Player{position=WR, name="andre", proj=16}
wr4 = Player{position=WR, name="sammy", proj=9}
te = Player{position=TE, name="scott", proj=10}
te2 = Player{position=TE, name="jay", proj=9}
def = Player{position=DEF, name="bills", proj=7}
k = Player{position=K, name="steve", proj=6}

-- maximum $ map sumPlayer $  filter validTeam $ teamCombos 9 [qb, qb2, rb, rb2, rb3, rb4, wr, wr2, wr3, wr4, te, te2, def, k]
