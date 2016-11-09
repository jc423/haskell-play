import Data.List
import System.IO

times4 :: Int -> Int
times4 x = x * 4

listTimes4 = map times4 [1, 2, 3, 4, 5]

multBy4 :: [Int] -> [Int]
multBy4 [] = []
multBy4 (x:xs) = times4 x : multBy4 xs

areStringsEqual :: String -> String -> Bool
areStringsEqual [] [] = True
areStringsEqual (x:xs) (y:ys) = x == y && areStringsEqual xs ys
areStringsEqual _ _ = False

doMult :: (Int -> Int) -> Int
doMult func = func 3

dbl1To10 = map (\x -> x * 2) [1..10]

doubleEvenNumber y =
  if (y `mod` 2 /= 0)
  then y
  else y * 2
