{-# LANGUAGE MultiWayIf #-}

-- var 17
-- task 1
-- IPZ - 41
-- Kosovan Ivan


module Task1 where

infixl 6 |-|
(|-|) :: Num a => a -> a -> a
(|-|) a b = abs a - abs b


mod' :: (Ord t, Num t) => t -> t -> t
mod' x y | y > x     =  x
         | otherwise =  mod' (x-y) y


recursion :: Integral a => a -> a -> a -> a
recursion n p q
    | p == q + 1    =   0
    | p < 0         =   recursion 0 0 q
    | mod' n 10 > 0 =   mod' n 10 + recursion (p+1) (p+1) q
    | n == 0        =   recursion (p+1) (p+1) q
    | otherwise     =   recursion (div n 10) p q


depth :: (Num a2, Integral a1) => a1 -> a1 -> a1 -> a2 -> a2
depth n p q d
    | p == q          = d
    | n == 0          = depth (p+1) (p+1) q (d+1)
    | mod' n 10 > 0   = depth (p+1) (p+1) q (d+1)
    | otherwise       = depth (div n 10) p q (d+1)


main :: IO()
main = do
    putStrLn "Enter p: "
    p'    <- getLine
    let p =  read p' :: Int

    putStrLn "Enter q, bigger then p: "
    q'    <- getLine
    let q =  read q' :: Int

    if | p > q -> error "q must be bigger than p (p < q)"
       | p <= q -> do
            
            -- обидва параметри менше нуля
            if | (p < 0 && q < 0) -> 
                    putStrLn ("Sum = 0, because all parameters are negative \n Recursion depth is " ++ show (2 * (p |-| q) + 1))

            -- p менше нуля, а q - більше
               | (p < 0 && q > 0) ->
                    putStrLn ("Sum = " ++ show (recursion p p q) ++ "\n Recursion depth is " ++ show ((2 * abs p) + depth 0 0 q 0))

            -- обидва параметри більше нуля
               | (p >= 0 && q > 0) -> do
                    let sum' = recursion p p q
                    putStrLn ("Sum = " ++ show sum')
                    putStrLn ("Recursion depth is " ++ show (depth p p q 0))