module Baseball (
  BaseballPlayer(Infielder, Pitcher, Catcher, Outfield),
  barryBonds,
  barryInOf,
  tomSmith,
  getBalance,
  RPS(Rock, Paper, Scissors),
  shoot
  ) where
import Data.List
import System.IO

data BaseballPlayer = Pitcher
                    | Catcher
                    | Infielder
                    | Outfield
                    deriving Show

barryBonds :: BaseballPlayer -> Bool
barryBonds Outfield = True

barryInOf = print(barryBonds Outfield)

data Customer = Customer String String Double
  deriving Show

tomSmith :: Customer
tomSmith = Customer "Tom Smith" " 123 Main" 20.50

getBalance :: Customer -> Double
getBalance (Customer _ _ x) = x

data RPS = Rock | Paper | Scissors

shoot :: RPS -> RPS -> String
shoot Paper Rock = "Paper Beats Rock"
shoot Paper Scissors = "Scissors beats Paper"
shoot Rock Paper = "Paper Beats Rock"
shoot Rock Scissors = "Rock beats Scissors"
shoot Scissors Rock = "Rock beats Scissors"
shoot Scissors Paper = "Scissors beats Paper"
shoot _ _ = "Unknown"

sumValue = putStrLn (show (1 + 2))
sumValue2 = putStrLn . show $ 1 + 2
