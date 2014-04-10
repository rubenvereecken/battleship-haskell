module Battleship where
import Control.Monad
import Control.Exception
import Utils


type Coordinate = (Int, Int)

data BattleShip = BattleShip { occupied :: [Coordinate] }
                               deriving (Show, Eq)

data Player = Player { name :: String,
                       ships :: [BattleShip],
                       shotsFired :: [Coordinate] }
                       deriving (Show)

-- IO Function that reads ships of given lengths, taking into account supplied ships
-- Defined recursively
readShips :: [Int] -> [BattleShip] -> IO [BattleShip]
readShips (x:xs) ships = do
                    boatString <- getLine
                    -- Read and parse ship
                    let newShip = BattleShip (Prelude.map read $ splitOn ';' boatString :: [Coordinate])
                    -- In case of overlap, retry
                    if or (Prelude.map (overlaps newShip) ships)
                    then do
                        putStrLn "Ship overlaps with existing one. Please try again."
                        moreBoats <- readShips (x:xs) ships
                        return $ moreBoats
                    else do
                        -- Check for ship validity
                        if not $ isValidShip newShip
                        then do
                            putStrLn "Invalid ship coordinates. Only horizontal and vertical allowed. Do try again."
                            moreBoats <- readShips (x:xs) ships
                            return $ moreBoats
                        else do
                            moreBoats <- readShips xs (ships ++ [newShip])
                            return $ [newShip] ++ moreBoats
readShips [] _ = do return []

-- game is over when either player has lost
gameOver :: Player -> Player -> Bool
gameOver p1 p2 = (hasLost p1) || (hasLost p2)

-- shooting a ship requires a coordinate and a ship
shootShip :: Coordinate -> BattleShip -> BattleShip
shootShip (x, y) (BattleShip coordinates) = BattleShip (Prelude.filter (/= (x, y)) coordinates)

-- Cleans out ships that have no coordinates left and have therefore sunk
cleanShips :: [BattleShip] -> [BattleShip]
cleanShips ships = Prelude.filter (/= BattleShip []) ships

-- Shoot a player at coordinate
-- Not responsible for printing out the result, instead returns boolean
shootPlayer :: Player -> Coordinate -> (Player, Bool) -- Player result, Bool didHit
shootPlayer (Player name ships fired) (x, y) = let newShips = Prelude.map (shootShip (x, y)) ships
                                               in (Player name newShips fired, ships /= newShips)

-- A player has lost when he has no ships left
hasLost :: Player -> Bool
hasLost (Player name ships _) = ships == []

-- Player 1 shoots count times at Player 2
-- Defined recursively, count decrements each time
doShots :: (Player, Player, Int) -> IO (Player, [Coordinate])
doShots ((Player name1 ships1 fired1), (Player name2 ships2 fired2), count)
    | count > 0 && not (gameOver (Player name1 ships1 fired1) (Player name2 ships2 fired2))
    = do
        -- Read in coordinate
        result <- try (getLine) :: IO (Either SomeException String)
        case result of
            Left ex -> do -- In case of IO error, try again
                putStrLn "Something went wrong. Try again."
                otherPath <- doShots ((Player name1 ships1 fired1), (Player name2 ships2 fired2), count)
                return otherPath
            Right coordinateString -> do
                -- Try parsing the coordinate. If it fails, try again.
                result <- try(evaluate(read coordinateString :: Coordinate)) :: IO (Either SomeException Coordinate)
                case result of
                    Left ex -> do
                        putStrLn "Discarded invalid coordinate. Try again."
                        otherPath <- doShots ((Player name1 ships1 fired1), (Player name2 ships2 fired2), count)
                        return otherPath
                    Right coordinate -> do
                        let (x, y) = coordinate
                        -- Check if coordinate is inside the field of 10x10
                        if (x > 9 || x < 0 || y > 9 || y < 0)
                        then do
                            putStrLn "Discarded invalid coordinate. Try again."
                            otherPath <- doShots ((Player name1 ships1 fired1), (Player name2 ships2 fired2), count)
                            return otherPath
                        else do
                            -- If all coordinate checks succeed, P1 may shoot P2
                            let (Player _ newShips _, didHit) = shootPlayer (Player name2 ships2 fired2) coordinate
                            -- Clean up dead, sunken ships
                            let cleanedShips = cleanShips newShips
                            if didHit
                            then putStrLn (name1 ++ " hit " ++ name2 ++ " at " ++ show coordinate ++ "!")
                            else putStrLn (name1 ++ " Misses.")
                            when (cleanedShips /= newShips) $ putStrLn "You sunk my battleship!"
                            -- Shoot again!
                            (player2, moreHits) <- doShots (Player name1 ships1 fired1, Player name2 cleanedShips fired2, (count - 1))
                            return (player2, [coordinate] ++ moreHits)
    | otherwise
    = do return ((Player name2 ships2 fired2), [])

-- IO Function that runs the game. One playGame call is one player's turn in which they may shoot multiple times.
-- It's always P1's turn. The function simply recursively calls itself, switching the players.
-- End result consists of the resulting players' shots fired and ships lost
playGame :: (Player, Player) -> IO (Player, Player)
playGame ((Player name1 ships1 fired1), (Player name2 ships2 fired2))
    -- If game over (base case), return
    | gameOver (Player name1 ships1 fired1) (Player name2 ships2 fired2)
    = do return ((Player name1 ships1 fired1), (Player name2 ships2 fired2))
    | otherwise
    = do
        putStrLn ("It's " ++ name1 ++ "'s turn. They get " ++ show (Prelude.length ships1) ++ " shots.")
        (newPlayer2, justShotAt) <- doShots ((Player name1 ships1 fired1), (Player name2 ships2 fired2), Prelude.length ships1)
        -- next round
        (nextPlayer1, nextPlayer2) <- playGame (newPlayer2, Player name1 ships1 (fired1 ++ justShotAt))
        return (nextPlayer1, nextPlayer2)

-- Utility function to determine whether a coordinate touches a ship
isInShips :: [BattleShip] -> Coordinate -> Bool
isInShips [] a = False
isInShips (x:xs) a = let BattleShip coordinates = x
                     in (if isIn coordinates a then True else isInShips xs a)

-- Utility function to determine overlap between two ships
overlaps :: BattleShip -> BattleShip -> Bool
overlaps (BattleShip c1) (BattleShip c2) = or (Prelude.map (isIn c1) c2)

-- Utility function for drawing the resulting play board
-- 'x' is for cell targeted
-- 'o' is for a live ship
-- '.' is empty
determineChar :: [Coordinate] -> [BattleShip] -> Coordinate -> Char
determineChar fired leftover coordinate = if isIn fired coordinate then 'x' else if isInShips leftover coordinate then 'o' else '.'

-- A playing board is represented as a [[Char]]; a character matrix
printField :: [String] -> IO ()
printField (x:xs) = do putStrLn x >> printField xs
printField [] = do return ()

-- Utility function to determine whether a ship's coordinates are valid.
-- Ships can be either horizontal of vertical and coordinates should be specified in order.
isValidShip :: BattleShip -> Bool
isValidShip (BattleShip ((x1,y1):(x2,y2):xs)) =
    let distance = (abs $ x1 - x2) + (abs $ y1 - y2)
    in and [(distance <= 1), (isValidShip $ BattleShip ((x2,y2):xs))]
isValidShip (BattleShip [(x1, y1)]) = True

-- Run this one
main = do
    -- Get basic input
    putStrLn "Who's Player 1"
    player1Name <- getLine
    putStrLn "Who's Player 2"
    player2Name <- getLine
    putStrLn $ player1Name ++ "'s ships"
    player1ships <- readShips [2..5] []
    putStrLn $ player2Name ++ "'s ships"
    player2ships <- readShips [2..5] []
    -- Create players
    let player1 = Player player1Name player1ships []
    let player2 = Player player2Name player2ships []

    -- Play the game, interactively
    (Player name1 ships1 fired1, Player name2 ships2 fired2) <- playGame (player1, player2)
    if hasLost $ Player name1 ships1 fired1
    then putStrLn (name1 ++ " lost.")
    else putStrLn (name2 ++ " lost.")

    -- Build playing fields for display purposes
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

