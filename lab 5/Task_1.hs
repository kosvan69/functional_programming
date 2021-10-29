module Task_1 where

-- !!! ответ: x = 1 !!!

import GHC.Real (Ratio)
import Data.Ratio ( Ratio, (%), denominator, numerator )

num1 :: Num a => a -> a
num1 x = 5*x -3

denom1 :: Num a => a -> a
denom1 x = x - 3

num2 :: Num a => a -> a
num2 x = 2*x - 3

denom2 :: Eq p => p -> p
denom2 x = x

left :: Integral a => a -> Ratio a
left x = num1 x % denom1 x

right :: Integral p => p -> Ratio p
right x = num2 x % denom2 x

equation :: Integral p => p -> Ratio p
equation x = left x - right x

sameDenom :: Integral a => a -> Ratio a
sameDenom x = ((num1 x * denom2 x) - (num2 x * denom1 x)) % (denom1 x * denom2 x)

solve :: Integral t => t -> t
solve x
  | num == 0 && denom /= 0 = x
  | otherwise = solve x+1
  where
    num = numerator (sameDenom x)
    denom = denominator (sameDenom x)

main :: IO ()
main = do
  putStrLn "Our eqaution: "
  putStrLn "(5*x-3) / (x-3) = (2*x-3) / x"

  let x' = solve 1
  putStrLn ("x = " ++ show x')
  putStrLn ("check: " ++ show(equation x'))