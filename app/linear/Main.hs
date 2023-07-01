module Main where


data Params = Params{   slope :: Double,
                        biais :: Double } deriving (Show)

predict:: [Double] -> Params ->[Double]
predict l p = map (\x -> x * (slope p) + (biais p) ) l


avgLst::[Double]->Double
avgLst l = (sum l) / (fromIntegral $ length l) 


loss:: [Double] -> [Double] -> Params -> Double
loss x y p = 
        let d = zipWith (-) (predict x p) y 
        in avgLst (map (\e -> e * e) d)


direction::[Double] -> [Double] -> Double -> Params -> Params
direction x y lr p
        | loss x y (Params ((slope p)+lr) (biais p)) < current_loss = Params ((slope p)+lr) (biais p)
        | loss x y (Params ((slope p)-lr) (biais p)) < current_loss = Params ((slope p)-lr) (biais p)
        | loss x y (Params (slope p) ((biais p)+lr)) < current_loss = Params (slope p) ((biais p)+lr)
        | loss x y (Params (slope p) ((biais p)-lr)) < current_loss = Params (slope p) ((biais p)-lr)
        | otherwise = Params (slope p) (biais p)
        where current_loss = loss x y p




train:: [Double] -> [Double] -> Integer -> Double -> Params -> Params
train x y acc eps p 
        | acc == 0 = p
        | otherwise = train x y (acc -1) eps (direction x y eps p)        


main :: IO ()
main = do
        putStrLn "Linear Regression"
        let x = [13.0,2.0,14.0,23.0]
        let y = [33.0, 16.0, 32.0, 51.0]
        let initial_p = Params 0.0 0.0
        let final_p = train x y 100000 0.01 initial_p        
        

        putStrLn $ show final_p 
        let r = predict [20.0] final_p
       
        putStrLn $ show r




