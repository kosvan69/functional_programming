module Task_2 where

import Data.Sequence as S

main :: IO()
main = do

    putStrLn "Our queue: "
    let q = S.fromList ['a'..'z']
    print q

    putStrLn "\n\nEnter the amount of elements: "
    a' <- getLine
    let amount = read a':: Int

    putStrLn "\n\nOur stack: "
    let s = S.take amount (S.reverse q)
    print s

    putStrLn "\n\nResult: "
    let res = S.reverse (S.take amount (S.reverse q)) S.>< s

    print res