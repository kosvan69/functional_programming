module Task_1 where

import Data.Vector as V

gcdMy :: Integral a => a -> a -> a
gcdMy n m
  | m > n = gcdMy m n
  | n `rem` m == 0 = m
  | otherwise = gcdMy m (n `rem` m)

main :: IO ()
main = do
  putStrLn "Our vectors: "
  let x = V.fromList [2, 3, 4, 100, 81]
  let y = V.fromList [5, 6, 7, 1000, 18]

  print x
  print y

  putStrLn "\nVectors GCD: "
  putStrLn "\nBuild func: "
  let res1 = V.zipWith gcdMy x y
  print res1
  
  putStrLn "\nMy func: "
  let res2 = V.fromList [gcdMy (x V.! i) (y V.! i) | i <-[0,1..V.length x - 1]]
  print res2