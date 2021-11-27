module Task_1 where

import Data.List ( sort )

main :: IO ()
main = do
  putStrLn "Enter amount of elements: "
  n'    <- getLine
  let n =  read n' :: Int

  let list_input = take n [(-n) `div` 2.. n `div` 2]

  putStrLn "Our list: "
  print list_input

  let list1 = [if x < 0 then x^2 else x | x <- list_input]

  putStrLn "\nList with squares of negative numbers: "
  print list1

  let list2 = reverse (sort list1)

  putStrLn "\nDescending sorted list: "
  print list2

  let sum3 = sum [x | x <- list2, even x]

  putStrLn "\nSum of even numbers: "
  print sum3