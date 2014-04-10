type Coordinate = (Int, Int)

determineChar :: [Coordinate] -> [Coordinate] -> Coordinate -> Char
determineChar fired leftover coordinate = if isIn fired coordinate then 'x' else if isIn leftover coordinate then 'o' else '.'


test = do
        let a = [[(x, y) | x <- [1..10]] | y <- [1..10]]
        let b = [c | c <- [1..10]]
        let fired = [(1,1), (5,5)]
        let res = map (map (determineChar fired [])) (a)
        print res

isIn :: Eq a => [a] -> a -> Bool
isIn [] a = False
isIn (x:xs) a = if (x == a) then True else isIn xs a