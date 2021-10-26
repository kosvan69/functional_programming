{-# LANGUAGE MultiWayIf #-}

-- var 17
-- task 2
-- IPZ - 41
-- Kosovan Ivan


module Task2 where

mod' :: (Ord t, Num t) => t -> t -> t
mod' x y | y > x     =  x
         | otherwise =  mod' (x-y) y

recursion :: Int -> Int -> Int -> IO ()
recursion n b dep
    | n == 1 = putStrLn ("\nDepth is " ++ show dep)
    | mod' n b == 0 = do
        putStr (show b ++ " ")
        recursion (div n b) b (dep+1)
    | otherwise = if b == 2 then recursion n (b+1) (dep+1) else recursion n (b+2) (dep+1)

main :: IO()
main = do
    putStrLn "Enter n: "
    n'    <- getLine
    let p =  read n' :: Int

    putStrLn "Multipliers: "

    recursion p 2 0