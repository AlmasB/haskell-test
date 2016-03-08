sum1 :: (Num a) => [a] -> a
sum1 list = foldl (\acc x -> acc * x) 1 list

sum2 :: Int
sum2 = length $ takeWhile (<=1000) $ scanl (+) 0 $ map sqrt [1..] 

main :: IO()
main = print $ sum2