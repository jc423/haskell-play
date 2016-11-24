replicatePlay :: Int -> a -> [a]
replicatePlay 0 _ = []
replicatePlay n x = x : replicatePlay (n - 1) x

toDigits :: Int -> [Int]
toDigits 0 = []
toDigits n = mod n 10 : toDigits (n `div` 10)

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:xs) = x : toDigits (2*y) ++  doubleEveryOther xs

validateCard :: Int -> Bool
validateCard n = (sum . doubleEveryOther $ toDigits n) `mod` 10 == 0


