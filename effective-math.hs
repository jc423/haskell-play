-- Meanie Genie

type Gem = Int
type GemGroup = [Int]

balance :: [GemGroup] -> [GemGroup]
balance groups
  | sum (head groups) > sum (head $ tail groups) = createGroups $ head groups
  | sum (head groups) < sum (head $ tail groups) = createGroups $ head $ tail groups
  | otherwise = createGroups $ last groups
  where createGroups = map (\x -> [x])

getGem :: [GemGroup] -> Gem
getGem gems = head $ head $ balance $ balance gems
