import Data.List
import System.IO

addMe :: Int -> Int -> Int
addMe x y = x + y

sumMe x y = x + y

addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x, y) (x2, y2) = (x + x2, y + y2)

whatAge :: Int -> String
whatAge 16 = "You can drive"
whatAge 18 = "You can vote"
whatAge 21 = "You can drink"
whatAge _ = "Fun's over"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

isEven :: Int -> Bool
isEven 0 = True
isEven x = isOdd (x - 1)


isOdd :: Int -> Bool
isOdd 0 = False
isOdd x = isEven (x - 1)


whatGrade :: Int -> String
whatGrade age
  | (age >= 5) && (age <= 6) = "Kindergarten"
  | (age > 6) && (age <= 7) = "First"
  | (age > 7) && (age <= 8) = "Second"
  | otherwise = "no school"


batAvg :: Double -> Double -> String
batAvg hits atBats
  | avg <= 0.200 = "Terrible"
  | avg <= 0.250 = "Okay"
  | avg <= 0.300 = "Solid"
  | otherwise = "Great"
  where avg = hits / atBats


getListItems :: [Int] -> String
getListItems [] = "Your list is empty"
getListItems (x:[]) = "Your list starts with " ++ show x
getListItems (x:y:[]) = " Your list contains " ++ show x ++ " and " ++ show y
getListItems (x:xs) = "Your first item is " ++ show x ++ " and rest is " ++ show xs

getFirstItem :: String -> String
getFirstItem [] = "Empty String"
getFirstItem all@(x:xs) = "The first item in your string " ++ show all ++ "is " ++ show x

