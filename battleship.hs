module Battleship where
import Data.Text

{-
data Coordinate = Coordinate { x :: Int,
                               y :: Int }
                               deriving (Eq)

instance Show Coordinate where
    show (Coordinate x y) = show ("(" ++ show x ++ "," ++ show y ++ ")")

-}

type Coordinate = (Int, Int)

data BattleShip = BattleShip { occupied :: [Coordinate] }
                               deriving (Show, Eq)

data Player = Player { name :: String,
                       ships :: [BattleShip] }
                       deriving (Show)

readShips :: [Int] -> IO [BattleShip]

readShips (x:xs) = do
                    boatString <- getLine
                    moreBoats <- readShips xs
                    return $ [BattleShip (Prelude.map read $ Prelude.map unpack $ splitOn (pack ";") (pack boatString) :: [Coordinate])] ++ moreBoats
readShips [] = do return []

gameOver :: Player -> Player -> Bool
gameOver (Player name1 ships1) (Player name2 ships2) = (ships1 == []) || (ships2 == [])

shootShip :: Coordinate -> BattleShip -> BattleShip
shootShip (x, y) (BattleShip coordinates) = BattleShip (Prelude.filter (/= (x, y)) coordinates)

cleanShips :: [BattleShip] -> [BattleShip]
cleanShips ships = Prelude.filter (/= BattleShip []) ships

shootPlayer :: Player -> Coordinate -> (Player, Bool) -- Player result, Bool didHit
shootPlayer (Player name ships) (x, y) = let newShips = cleanShips $ Prelude.map (shootShip (x, y)) ships
                                         in (Player name newShips, ships /= newShips)

-- Player 1 shoots count times at Player 2
doShots :: (Player, Player, Int) -> IO (Player, [Coordinate])
doShots ((Player name1 ships1), (Player name2 ships2), count)
    | count > 0
    = do
        coordinateString <- getLine
        let coordinate = read coordinateString :: Coordinate
        let (changedPlayer, didHit) = shootPlayer (Player name2 ships2) coordinate
        (player2, moreHits) <- doShots ((Player name1 ships1), changedPlayer, (count - 1))
        let returnVal = (player2, [coordinate] ++ moreHits)
        if didHit
        then print (name1 ++ " hit " ++ name2 ++ " at " ++ show coordinate) >> return returnVal
        else return returnVal
    | otherwise
    = do return ((Player name2 ships2), [])

playGame :: (Player, Player) -> IO (Player, Player, [Coordinate], [Coordinate])
playGame ((Player name1 ships1), (Player name2 ships2))
    | gameOver (Player name1 ships1) (Player name2 ships2)
    = do return ((Player name1 ships1), (Player name2 ships2), [], [])
    | otherwise
    = do
        (newPlayer2, justShotAt) <- doShots ((Player name1 ships1), (Player name2 ships2), Prelude.length ships1)
        -- next round
        (nextPlayer1, nextPlayer2, nextShotAt1, nextShotAt2) <- playGame (newPlayer2, Player name1 ships1)
        return (nextPlayer1, nextPlayer2, nextShotAt1, nextShotAt2 ++ justShotAt)


test :: Coordinate -> Coordinate
test (a, b) = (a, b)

main = do
    putStrLn "Who's the guy on the left"
    player1Name <- getLine
    putStrLn "Who's the guy on the right"
    player2Name <- getLine
    putStrLn $ player1Name ++ " VS " ++ player2Name
    putStrLn "Player 1's ships"
    player1ships <- readShips [2..5]
    putStrLn "Player 2's ships"
    player2ships <- readShips [2..5]
    let player1 = Player player1Name player1ships
    let player2 = Player player2Name player2ships
    --Z(player1Final, player2Final, targeted1, targeted2) <- playGame (player1, player2)

    putStrLn "Game's over go home"

