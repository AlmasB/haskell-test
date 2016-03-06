isPrime :: Int -> Bool
isPrime n = null $ filter p [2..n-1]
            where p x = mod n x == 0

main :: IO()
main = print $ take 100 $ filter isPrime [100..]
