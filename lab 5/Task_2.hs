module Task_2 where

import Data.Complex ( Complex((:+)), polar, magnitude, phase )

add :: Num a => a -> a -> a
add x y = x + y

sub :: Num a => a -> a -> a
sub x y = x - y

mult :: Num a => a -> a -> a
mult x y = x * y

div' :: Fractional a => a -> a -> a
div' x y = x / y

toTrig :: RealFloat a => Complex a -> Complex a
toTrig x = r * cos phi :+ r * sin phi
    where
        r = magnitude x
        phi = phase x

main :: IO ()
main = do

    let z1 = 1 :+ 5
    let z2 = 2 :+ 7

    putStrLn ("Додавання в алгебр формi:  " ++ show (add z1 z2))
    putStrLn ("Вiднiмання в алгебр формi:  " ++ show (sub z1 z2))
    putStrLn ("Множення в алгебр формi:  " ++ show (mult z1 z2))
    putStrLn ("Дiлення в алгебр формi:  " ++ show (div' z1 z2))

    let z1trig = toTrig z1
    let z2trig = toTrig z2

    putStrLn ("\n\nДодавання в геом формi:  " ++ show (add z1trig z2trig))
    putStrLn ("Вiднiмання в геом формi:  " ++ show (sub z1trig z2trig))
    putStrLn ("Множення в геом формi:  " ++ show (mult z1trig z2trig))
    putStrLn ("Дiлення в геом формi:  " ++ show (div' z1trig z2trig))