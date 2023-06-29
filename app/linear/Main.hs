module Main where


predict:: [Double] -> Double -> Double ->[Double]
predict l w b = map (\x -> x * w + b) l


avgLst::[Double]->Double
avgLst l = (sum l) / (fromIntegral $ length l) 


loss:: [Double] -> [Double] -> Double -> Double -> Double
loss x y w b = 
        let d = zipWith (-) (predict x w b) y 
        in avgLst (map (\e -> e * e) d)



main :: IO ()
main = do
        putStrLn "Linear Regression"
        let x = [1,2,3,4,5,6]
        let r = predict x 2 3
        putStrLn $ show r
        putStrLn $ show $ avgLst x



