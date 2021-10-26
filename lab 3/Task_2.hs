module Task_2 where

f :: Floating a => a -> a
f x = x^4 * exp (-x^2)

-- правых прямоугольников
rightRectanglesIntegration :: Double -> Double -> Integer -> (Double -> Double) -> Double
rightRectanglesIntegration xmin xmax intervals f =
    let step = stepIntegration xmin xmax intervals
        s  = sumIntegration (xmin + step) xmax step f
    in s * step
 
-- трапеций
trapezoidalIntegration :: Double -> Double -> Integer -> (Double -> Double) -> Double
trapezoidalIntegration xmin xmax intervals f =
    let step = stepIntegration xmin xmax intervals
        s  = sumIntegration (xmin + step) (xmax - step) step f
    in (s + (f xmin + f xmax)/2) * step
 
-- вспомогательные для вызова
sumIntegration :: Double -> Double -> Double -> (Double -> Double) -> Double
sumIntegration xFirst xLast step f = sum $ map f [xFirst, xFirst + step .. xLast]
 
stepIntegration :: Double -> Double -> Integer -> Double 
stepIntegration xmin xmax intervals = (xmax - xmin) / fromInteger intervals

main :: IO ()
main = do

    let rRect = rightRectanglesIntegration 0 pi 100000 f
    let trap = trapezoidalIntegration 0 pi 100000 f

    let dif = abs (rRect - trap)
    
    putStrLn ""
    putStrLn ""
    putStrLn "+----------------------------+----------------------------+------------------------------+"
    putStrLn "|       Right rectangle      |        Trapezoidal         |           Difference         |"
    putStrLn "+----------------------------+----------------------------+------------------------------+"
    putStrLn ("|     " ++ show rRect ++ "     |     " ++ show trap ++ "     |     " ++ show dif ++ "     |")
    putStrLn "+----------------------------+----------------------------+------------------------------+"