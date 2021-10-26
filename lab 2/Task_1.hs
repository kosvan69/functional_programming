atanMy :: Floating a => a -> a
atanMy x = atanMy1 x- atanMy1 x+ id
    where id = atan x

y' :: (Ord a, Floating a) => a -> a
y' x
    | x >= 1 = atan x / atan x-5
    | x >= 0 && x < 1 = atan x + atan 2*x


y :: (Ord a, Floating a) => a -> a
y x
    | x >= 1 = atanMy x / atanMy x-5
    | x >= 0 && x < 1 = atanMy x + atanMy 2*x


taylor :: (Num a1, Num a2, Enum a2) => (t -> a1) -> (t -> a2 -> a1) -> t -> a1
taylor k_ini k_n x = k_ini x * Prelude.foldr ((.) . app k_n x) id [2..100] 1
          where app k x n = (1+) . (k x n *)


atanMy1 :: Fractional a1 => a1 -> a1
atanMy1 x = taylor id k_n x
          where k_n x n = - x^2 / fromIntegral (2*n+1)


main :: IO()
main = do
    putStrLn "+-----+------------------+-----------------------------+-------+"
    putStrLn "|  n  |  Build function  | Function with Taylor series | Error |"
    putStrLn "+-----+------------------+-----------------------------+-------+"
    putStrLn ("|  0  |       " ++ show (y' 0) ++ "        |            " ++ show (y 0) ++"              |  " ++ show (y' 0 - y 0) ++ "  |")
    putStrLn "+-----+------------------+-----------------------------+-------+"
    putStrLn ("| 0.5 |" ++ show (y' 0.5) ++ "|      " ++ show (y 0.5) ++ "     |  " ++ show (y' 0.5 - y 0.5) ++ "  |")
    putStrLn "+-----+------------------+-----------------------------+-------+"
    putStrLn ("|  1  |       " ++ show (y' 1) ++ "       |             " ++ show (y 1) ++ "            |  " ++ show (y' 1 - y 1) ++ "  |")
    putStrLn "+-----+------------------+-----------------------------+-------+"
    putStrLn ("| 1.5 |       " ++ show (y' 1.5) ++ "       |             " ++ show (y 1.5) ++ "            |  " ++ show (y' 1.5 - y 1.5) ++ "  |")
    putStrLn "+-----+------------------+-----------------------------+-------+"
    putStrLn ("|  2  |       " ++ show (y' 2) ++ "       |             " ++ show (y 2) ++ "            |  " ++ show (y' 2 - y 2) ++ "  |")
    putStrLn "+-----+------------------+-----------------------------+-------+"
    putStrLn ("| 2.5 |       " ++ show (y' 2.5) ++ "       |             " ++ show (y 2.5) ++ "            |  " ++ show (y' 2.5 - y 2.5) ++ "  |")
    putStrLn "+-----+------------------+-----------------------------+-------+"
    putStrLn ("|  3  |       " ++ show (y' 3) ++ "       |             " ++ show (y 3) ++ "            |  " ++ show (y' 3 - y 3) ++ "  |")
    putStrLn "+-----+------------------+-----------------------------+-------+"