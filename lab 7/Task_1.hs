module Task_1 where

import System.Random ( randomRIO )
import System.IO as IO ( hClose, openFile, IOMode(ReadWriteMode) )
import Data.Char as C (isNumber)
import Data.List as L ()
import qualified System.IO.Strict as StrictIO

-- функция подсчета цифр в строке
countInts :: [Char] -> Int
countInts = sum . map (fromEnum . C.isNumber)

main :: IO ()
main = do
  -- считываем число n
  putStrLn "Enter n: "
  n' <- getLine
  let n = read n' :: Int

  -- файл
  let file = "text.txt"

  -- создаем строку для записи
  num1 <- randomRIO (0, 100 :: Int)
  num2 <- randomRIO (0, 100 :: Int)
  let str = show num1 ++ ['a'..'z'] ++ show num2 ++ "\n"

  -- записываем в файл
  writeFile file (take (n * length str) (cycle str))
  putStrLn "\nCheck output file!"

  -- считываем с файла
  content <- StrictIO.readFile "text.txt"

  -- вывести содержимое
  putStrLn "\nContent: "
  putStr content

  -- количество 
  let amountNumbers = countInts content

  -- вывод результата
  putStrLn "\nAmount of numbers: "
  print amountNumbers