module Task_1 where

import Data.List ( iterate' )

-- 3x - cosx - 1 = 0
-- f(x) = 3x - cosx - 1
-- f'(x) = sin(x) + 3

f :: Double -> Double
f x = 3*x - cos x - 1

fDeriv :: Double -> Double
fDeriv x = sin x + 3

eps :: Double
eps = 1e-6

lambda :: (Double -> Double) -> Double -> Double
lambda deriv = recip . deriv -- recip -> 1/x

------------------------------------------------------------
iter :: Double -> (Double -> Double) -> (Double -> Double) -> Double -> p -> Double
iter eps fun deriv start end = fst $ head $ dropWhile (not . goodEnough) pairs
    where
      goodEnough (x, y) = abs (x - y) < eps
      pairs             = zip (tail intermediates) intermediates
      intermediates     = iterate phi start
      phi x             = x - lambda deriv start * fun x

------------------------------------------------------------

newton :: (Fractional b, Ord b) => b -> (b -> b) -> (b -> b) -> b -> p -> b
newton epsilon func deriv x0 end = fst . head $ dropWhile pred pairs
  where
    pred (xn, xn1) = (abs xn - xn1) > epsilon -- определяем достигнута ли точность
    next xn = xn - func xn / deriv xn -- новое приближение
    iters   = iterate' next x0        -- бесконечный список итераций
    pairs   = zip iters (tail iters)  -- бесконечный список пар итераций


main :: IO ()
main = do
  putStrLn "Enter a: "
  a'    <- getLine
  let a =  read a' :: Double

  putStrLn "Enter b: "
  b'    <- getLine
  let b =  read b' :: Double

  let iter_x   = iter eps f fDeriv a b
  let newton_x = newton eps f fDeriv a b

  let iter_y = f iter_x
  let newton_y = f newton_x

  putStrLn ""
  putStrLn ""
  putStrLn "+----------------------+------------------------+-------------------------+-------------------------+"
  putStrLn "|     Iteration x*     |        Newton x*       |     Iteration f(x*)     |       Newton f(x*)      |"
  putStrLn "+----------------------+------------------------+-------------------------+-------------------------+"
  putStrLn ("|  " ++ show iter_x ++ "  |   " ++ show newton_x ++ "   |  " ++ show iter_y ++ "  |   " ++ show newton_y ++ "   |")
  putStrLn "+----------------------+------------------------+-------------------------+-------------------------+"
  putStrLn ""
  putStrLn ("Рiзниця в значеннях х* мiж методами: " ++ show (abs (iter_x - newton_x)))
  putStrLn ("Рiзниця в значеннях f(х*) мiж методами: " ++ show (abs (iter_y - newton_y)))