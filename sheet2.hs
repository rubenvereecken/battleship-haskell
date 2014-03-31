readIntFromChar        ::  Char -> Int
readIntFromChar b      =   read [b]

extractDigits       ::  String -> [Int]
extractDigits a     = map readIntFromChar a
