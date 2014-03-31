init    :: [a] -> [a]
init a  = take (subtract 1 (length a)) a

last        :: [a] -> a
last [a]    = a
last (a:b)  = Main.last b

safetail    :: 
