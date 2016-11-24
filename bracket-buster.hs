data Team = Team{name::String, ppg::Int, pag::Int} deriving Show
data Bracket = Bracket{children::[Bracket]} | Game{home::Team, away::Team} deriving Show

teamA = Team {name="A", ppg=100, pag=90}
teamB = Team {name="B", ppg=90, pag=100}
teamC = Team {name="C", ppg=100, pag=90}
teamD = Team {name="D", ppg=190, pag=100}
teamE = Team {name="E", ppg=100, pag=90}
teamF = Team {name="F", ppg=90, pag=100}
teamG = Team {name="G", ppg=200, pag=90}
teamH = Team {name="H", ppg=290, pag=100}

game1 = Game {home=teamA, away=teamB}
game2 = Game {home=teamC, away=teamD}
game3 = Game {home=teamE, away=teamF}
game4 = Game {home=teamG, away=teamH}

bracketWest = Bracket{children=[game1, game2]}
bracketEast = Bracket{children=[game2]}
bracketNorth = Bracket{children=[game3]}
bracketSouth = Bracket{children=[game4]}
bracketRegional1 = Bracket{children=[bracketWest, bracketEast]}
bracketRegional2 = Bracket{children=[bracketNorth, bracketSouth]}
bracketFinal = Bracket{children=[bracketRegional1, bracketRegional2]}

bracketWinner:: Bracket->Team
bracketWinner bracket = case bracket of
                       Game h a -> betterTeam h a
                       Bracket children -> case children of 
                                             (x:[]) -> bracketWinner x
                                             (x:xs) -> betterTeam (bracketWinner x) (bracketWinner (Bracket{children=xs}))
                                             otherwise -> error "unmatched bracket winner"


betterTeam::Team->Team->Team
betterTeam a b
  | ppg a > ppg b = a
  | otherwise = b
