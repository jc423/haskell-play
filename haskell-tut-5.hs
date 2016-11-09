import Data.List
import System.IO

data Employee = Employee {name :: String,
                         position :: String,
                         idNum :: Int
                         } deriving (Eq, Show)

samSmith = Employee {name = "Sam", position = "manager", idNum = 1}
kateSmith = Employee {name = "Kate", position = "clerk", idNum = 2}

isSamKate = samSmith == kateSmith
samSmithData = show samSmith

data ShirtSize = S | M | L

instance Eq ShirtSize where
  S == S = True
  M == M = True
  L == L = True
  _ == _ = False

instance Show ShirtSize where
  show S = "Small"
  show M = "Medium"
  show L = "Large"

smallAvail = S `elem` [S, M, L]
theSize = show S


class MyEq a where
  areEqual :: a -> a -> Bool

instance MyEq ShirtSize where
  areEqual S S = True
  areEqual M M = True
  areEqual L L = True


sayHello = do
  putStrLn "what is your name?"
  name <- getLine
  putStrLn $ "Hello " ++ name

writeToFile = do
  theFile <- openFile "test.txt" WriteMode
  hPutStrLn theFile ("Random Line of text")
  hClose theFile

readFromFile = do
  theFile <- openFile "test.txt" ReadMode
  contents <- hGetContents theFile
  putStr contents
  hClose theFile

