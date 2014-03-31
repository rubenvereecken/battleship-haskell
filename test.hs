double x = x + x
quadruple x = double(double(x))

-- ******************************************************--

add :: (Int, Int) -> Int
add (x,y) = x + y

add2 :: Int -> (Int -> Int)
add2 x y = x + y

add3 :: Int -> Int -> Int
add3 x y = x + y

-- ******************************************************-- 

signum1 :: Int -> Int              --Sign of the number --
signum1 n = if n < 0 then -1 else if n == 0 then 0 else 1

signum2 :: Int -> Int
signum2 n	| n < 0		= -1
			| n == 0	= 0
			| otherwise	= 1		
			
-- ******************************************************-- 			
			
(&&) :: Bool -> Bool -> Bool
False && _ = False 
True && b = b 