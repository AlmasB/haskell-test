collatzSeq :: Int -> [Int]
collatzSeq n
            | n == 1 = [1]
            | even n = n : collatzSeq (div n 2)
            | otherwise = n : collatzSeq (n*3+1)

solution :: Int -> [Int] -> Int
solution chainLen list = length $ filter (>chainLen) $ map (length . collatzSeq) list

solution' :: Int -> [Int] -> Int
solution' chainLen list = length $ filter (\x -> length x > chainLen) $ map collatzSeq list

main :: IO()
main = do
        print $ solution 15 [1..100]
        print $ solution' 15 [1..100]
