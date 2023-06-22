module Main where

predict:: [Double] -> Double -> Double ->[Double]
predict l w b = map (\x -> x * w + b) l

loss:: [Double] -> [Double] -> Double -> Double -> [Double]
loss l y w b = 
        let d = zipwidth (-) (predict x w b) y
        in 

main :: IO ()
main = do
        putStrLn "Linear Regression"
        let x = [1,2,3,4,5,6]
        let r = predict x 2 3
        putStrLn $ show r


