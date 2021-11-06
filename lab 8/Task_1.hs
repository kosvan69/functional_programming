module Task_1 where

import Data.List ()

intArray :: (Eq t, Num t) => t -> IO [Double]
intArray 0 = return []
intArray x = do
    str <- getLine
    nextInt <- intArray (x - 1)
    let int = read str :: Double
    return (int:nextInt)

coeff :: [Double] -> [Double]
coeff bs = zipWith (/) bs [n,n-1.0..1.0]  ++ [5.0] where n = fromIntegral $ length bs

toTuples :: [Double] -> [Int] -> [(Int, Int)]
toTuples coefs xs = [(round (coefs!!n) :: Int, xs!!n) | n <- [0..length coefs-1]]

showPolynomial :: [(Int,Int)] -> String
showPolynomial [] = []
showPolynomial (x:xs) = show2 x ++ concatMap showN xs
   where show2 (v,0)  = show  v
         show2 (v,1)  = show1 v ++ "x"
         show2 (v,p)  = show1 v ++ "x^" ++ show p
         show1   1    = ""
         show1 (-1)   = "-"
         show1   v    = show v
         showN (v,p) | v <0 = show2 (v,p)
                     |otherwise = '+' : show2 (v,p)

main :: IO ()
main = do
  putStrLn "Enter n: "
  n' <- getLine
  let n = read n' :: Int
  x <- intArray n
  let xs = [n, n-1..0]
  putStrLn "\n\nOur polinom: "
  putStrLn $ showPolynomial (toTuples (1 : init (coeff x)) xs)