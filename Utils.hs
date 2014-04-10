module Utils where

isIn :: Eq a => [a] -> a -> Bool
isIn [] a = False
isIn (x:xs) a = if (x == a) then True else isIn xs a

-- Made from Data.List's `words`
splitOn :: Char -> String -> [String]
splitOn (c) (s) =  case dropWhile (== c) s of
                "" -> []
                s' -> w : splitOn c s''
                      where (w, s'') =
                             break (== c) s'