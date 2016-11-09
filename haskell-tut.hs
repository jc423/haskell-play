import Data.List

import System.IO

primeNumber = [3, 5, 7, 11]
morePrime = primeNumber ++ [13, 17, 19, 23, 29]

favNums = 2 : 7 : 21 : 66 : []

multList = [[3,5,7], [11, 13, 17]]

morePrimes2 = 2 : morePrime
lenPrime = length morePrime

revPrime = reverse morePrime
isListEmpty = null morePrime
secondPrime = morePrime !! 1
firstPrime = head morePrime
lastPrime = last morePrime
allButLastPrime = init morePrime
allButFirstPrime = tail morePrime
first3Prime = take 3 morePrime
removePrimes = drop 3 morePrime
is7InList = 7 `elem` morePrime
maxPrime = maximum morePrime
minPrime = minimum morePrime
productPrimes = product morePrime
sumPrimes = sum morePrime

zeroToTen = [0..10]
evenList = [2,4..100]
charList = ['A','B'..'Z']

infinMult10 = [10,20..]
many2s = take 10 (repeat 2)
many3s = replicate 10 3
cycleList = take 10 (cycle [1, 2, 3, 4, 5])
listTime2 = [x * 2 | x <- [1..10]]
listTime3Filtered = [x * 3 | x <- [1..20], x * 3 <= 50]
diviBy9Or13 = [x | x <- [1..500], mod x 9 == 0, mod x 13 == 0]
sumOfLists = zipWith (+) [1, 2, 3, 4, 5] [2, 4, 6, 8, 10]
evensUpTo20 = takeWhile (<= 20) [2, 4..]

multOfList = foldl (*) 1 [2, 3, 4, 5]
multOfListR = foldl (*) 1 [2, 3, 4, 5]

pow3List = [3^n | n <- [1..10]]
multTable = [[x * y | x <- [1..12]] | y <- [1..12]]

randTuple = (1, "Rand Tuple")
bobSmith = ("Bob Smith", 52)
bobName = fst bobSmith
bobAge = snd bobSmith
names = ["Bob", "Mary", "Tom"]
ages = [22,12,33]
nameAndAges = zip names ages

