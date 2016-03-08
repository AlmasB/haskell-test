isPrime :: Int -> Bool
isPrime n = null $ filter (\x -> mod n x == 0) [2..n-1]

main :: IO()
main = print $ take 100 $ filter isPrime [100..]
