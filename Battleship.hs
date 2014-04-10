module Battleship where
import Control.Monad
import Control.Exception
import Utils

{-
data Coordinate = Coordinate { x :: Int,
                               y :: Int }
                               deriving (Eq)

instance Show Coordinate where
    show (Coordinate x y) = show ("(" ++ show x ++ "," ++ show y ++ ")")

-}

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

type Coordinate = (Int, Int)

data BattleShip = BattleShip { occupied :: [Coordinate] }
                               deriving (Show, Eq)

data Player = Player { name :: String,
                       ships :: [BattleShip],
                       shotsFired :: [Coordinate] }
                       deriving (Show)

readShips :: [Int] -> [BattleShip] -> IO [BattleShip]
readShips (x:xs) ships = do
                    boatString <- getLine
                    let newShip = BattleShip (Prelude.map read $ splitOn ';' boatString :: [Coordinate])
                    if or (Prelude.map (overlaps newShip) ships)
                    then do
                        putStrLn "Ship overlaps with existing one. Please try again."
                        moreBoats <- readShips (x:xs) ships
                        return $ moreBoats
                    else do
                        if not $ isValidShip newShip
                        then do
                            putStrLn "Invalid ship coordinates. Only horizontal and vertical allowed. Do try again."
                            moreBoats <- readShips (x:xs) ships
                            return $ moreBoats
                        else do
                            moreBoats <- readShips xs (ships ++ [newShip])
                            return $ [newShip] ++ moreBoats
readShips [] _ = do return []

gameOver :: Player -> Player -> Bool
gameOver p1 p2 = (hasLost p1) || (hasLost p2)

shootShip :: Coordinate -> BattleShip -> BattleShip
shootShip (x, y) (BattleShip coordinates) = BattleShip (Prelude.filter (/= (x, y)) coordinates)

cleanShips :: [BattleShip] -> [BattleShip]
cleanShips ships = Prelude.filter (/= BattleShip []) ships

shootPlayer :: Player -> Coordinate -> (Player, Bool) -- Player result, Bool didHit
shootPlayer (Player name ships fired) (x, y) = let newShips = Prelude.map (shootShip (x, y)) ships
                                         in (Player name newShips fired, ships /= newShips)

hasLost :: Player -> Bool
hasLost (Player name ships _) = ships == []

-- Player 1 shoots count times at Player 2
doShots :: (Player, Player, Int) -> IO (Player, [Coordinate])
doShots ((Player name1 ships1 fired1), (Player name2 ships2 fired2), count)
    | count > 0 && not (gameOver (Player name1 ships1 fired1) (Player name2 ships2 fired2))
    = do
        result <- try (getLine) :: IO (Either SomeException String)
        case result of
            Left ex -> do
                putStrLn "Something went wrong. Try again."
                otherPath <- doShots ((Player name1 ships1 fired1), (Player name2 ships2 fired2), count)
                return otherPath
            Right coordinateString -> do
                result <- try(evaluate(read coordinateString :: Coordinate)) :: IO (Either SomeException Coordinate)
                case result of
                    Left ex -> do
                        putStrLn "Discarded invalid coordinate. Try again."
                        otherPath <- doShots ((Player name1 ships1 fired1), (Player name2 ships2 fired2), count)
                        return otherPath
                    Right coordinate -> do
                        let (x, y) = coordinate
                        if (x > 9 || x < 0 || y > 9 || y < 0)
                        then do
                            putStrLn "Discarded invalid coordinate. Try again."
                            otherPath <- doShots ((Player name1 ships1 fired1), (Player name2 ships2 fired2), count)
                            return otherPath
                        else do
                            let (Player _ newShips _, didHit) = shootPlayer (Player name2 ships2 fired2) coordinate
                            let cleanedShips = cleanShips newShips
                            if didHit
                            then putStrLn (name1 ++ " hit " ++ name2 ++ " at " ++ show coordinate ++ "!")
                            else putStrLn (name1 ++ " Misses.")
                            when (cleanedShips /= newShips) $ putStrLn "You sunk my battleship!"
                            (player2, moreHits) <- doShots (Player name1 ships1 fired1, Player name2 cleanedShips fired2, (count - 1))
                            return (player2, [coordinate] ++ moreHits)
    | otherwise
    = do return ((Player name2 ships2 fired2), [])

playGame :: (Player, Player) -> IO (Player, Player)
playGame ((Player name1 ships1 fired1), (Player name2 ships2 fired2))
    | gameOver (Player name1 ships1 fired1) (Player name2 ships2 fired2)
    = do
        return ((Player name1 ships1 fired1), (Player name2 ships2 fired2))
    | otherwise
    = do
        putStrLn ("It's " ++ name1 ++ "'s turn. They get " ++ show (Prelude.length ships1) ++ " shots.")
        (newPlayer2, justShotAt) <- doShots ((Player name1 ships1 fired1), (Player name2 ships2 fired2), Prelude.length ships1)
        -- next round
        (nextPlayer1, nextPlayer2) <- playGame (newPlayer2, Player name1 ships1 (fired1 ++ justShotAt))
        return (nextPlayer1, nextPlayer2)

isInShips :: [BattleShip] -> Coordinate -> Bool
isInShips [] a = False
isInShips (x:xs) a = let BattleShip coordinates = x
                     in (if isIn coordinates a then True else isInShips xs a)

overlaps :: BattleShip -> BattleShip -> Bool
overlaps (BattleShip c1) (BattleShip c2) = or (Prelude.map (isIn c1) c2)

determineChar :: [Coordinate] -> [BattleShip] -> Coordinate -> Char
determineChar fired leftover coordinate = if isIn fired coordinate then 'x' else if isInShips leftover coordinate then 'o' else '.'

printField :: [String] -> IO ()
printField (x:xs) = do putStrLn x >> printField xs
printField [] = do return ()

isValidShip :: BattleShip -> Bool
isValidShip (BattleShip ((x1,y1):(x2,y2):xs)) =
    let distance = (abs $ x1 - x2) + (abs $ y1 - y2)
    in and [(distance <= 1), (isValidShip $ BattleShip ((x2,y2):xs))]
isValidShip (BattleShip [(x1, y1)]) = True

main = do
    putStrLn "Who's Player 1"
    player1Name <- getLine
    putStrLn "Who's Player 2"
    player2Name <- getLine
    putStrLn $ player1Name ++ " VS " ++ player2Name
    putStrLn $ player1Name ++ "'s ships"
    player1ships <- readShips [2..5] []
    putStrLn $ player2Name ++ "'s ships"
    player2ships <- readShips [2..5] []
    let player1 = Player player1Name player1ships []
    let player2 = Player player2Name player2ships []

    (Player name1 ships1 fired1, Player name2 ships2 fired2) <- playGame (player1, player2)
    if hasLost $ Player name1 ships1 fired1
    then putStrLn (name1 ++ " lost.")
    else putStrLn (name2 ++ " lost.")

    let player1field = [[(x, y) | x <- [0..9]] | y <- [0..9]]
    let player2field = [[(x, y) | x <- [0..9]] | y <- [0..9]]
    let player1result = Prelude.map (Prelude.map (determineChar fired2 ships1)) (player1field)
    let player2result = Prelude.map (Prelude.map (determineChar fired1 ships2)) (player2field)

    putStrLn "==========="
    putStrLn "=  Recap  ="
    putStrLn "==========="
    putStrLn $ name1 ++ "'s field:"
    printField player1result

    putStrLn $ name2 ++ "'s field:"
    printField player2result

    putStrLn "Game's over go home"

