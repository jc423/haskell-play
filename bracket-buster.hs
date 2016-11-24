data Team = Team {name::String, ppg::Int, pag::Int} deriving Show
data Bracket = Bracket {children::[Bracket]} | Game {home::Team, away::Team} deriving Show

bracketWinner::Bracket->Team
bracketWinner bracket = case bracket of
                          Game h a -> betterTeam h a
                          Bracket childs -> case childs of 
                                                (x:[]) -> bracketWinner x
                                                (x:xs) -> betterTeam (bracketWinner x) (bracketWinner (Bracket{children=xs}))
                                                otherwise -> error "missing pattern for bracket"

betterTeam::Team->Team->Team
betterTeam a b
  | ppg a > ppg b = a
  | otherwise = b

teamA = Team {name="A", ppg=100, pag=90}
teamB = Team {name="B", ppg=90, pag=100}
teamC = Team {name="C", ppg=101, pag=90}
teamD = Team {name="D", ppg=170, pag=100}
teamE = Team {name="E", ppg=100, pag=90}
teamF = Team {name="F", ppg=90, pag=100}
teamG = Team {name="G", ppg=200, pag=90}
teamH = Team {name="H", ppg=140, pag=100}
teamI = Team {name="I", ppg=100, pag=90}
teamJ = Team {name="J", ppg=90, pag=100}
teamK = Team {name="K", ppg=100, pag=90}
teamL = Team {name="L", ppg=190, pag=100}
teamM = Team {name="M", ppg=100, pag=90}
teamN = Team {name="N", ppg=90, pag=100}
teamO = Team {name="O", ppg=200, pag=90}
teamP = Team {name="P", ppg=289, pag=100}
game1 = Game {home=teamA, away=teamB}
game2 = Game {home=teamC, away=teamD}
game3 = Game {home=teamE, away=teamF}
game4 = Game {home=teamG, away=teamH}
game5 = Game {home=teamI, away=teamJ}
game6 = Game {home=teamK, away=teamL}
game7 = Game {home=teamM, away=teamN}
game8 = Game {home=teamO, away=teamP}
bracketWest = Bracket{children=[game1,game2]}
bracketEast = Bracket{children=[game3,game4]}
bracketNorth = Bracket{children=[game5,game6]}
bracketSouth = Bracket{children=[game7,game8]}
bracketRegional1 = Bracket{children=[bracketWest, bracketEast]}
bracketRegional2 = Bracket{children=[bracketNorth, bracketSouth]}
bracketFinal = Bracket{children=[bracketRegional1, bracketRegional2]}

-- winner of final
finalWinner = bracketWinner bracketFinal

-- winner of bracketWest winner and bracketEast winner
bracketWestAndEastWinner = bracketWinner $ Game {home=(bracketWinner bracketSouth), away=(bracketWinner bracketEast)}

