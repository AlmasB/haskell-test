import System.Random (StdGen, next, getStdGen, randomR, newStdGen, randomRIO)
import Data.List
import Data.Set (Set, fromList)
import qualified Data.Set as Set

quicks :: (Ord a) => [a] -> [a]
quicks [] = []
quicks (x:xs) = (quicks (filter (<=x) xs)) ++ [x] ++ (quicks (filter (>x) xs))

randomlist :: Int -> StdGen -> [Int]
randomlist n = take n . unfoldr (Just . randomR (1, 10))

largestDiv :: Int -> Int -> Int
largestDiv n d = head [x | x <- series n, mod x d == 0]

largestDiv' n d = head $ filter p $ series n
                    where p x = mod x d == 0

series:: Int -> [Int]
series n = [n,n-1..]

main = do
    seed  <- newStdGen
    let list = randomlist 10 seed
    let sorted = quicks list
    print list
    print sorted
    print $ reverse sorted
    print $ take 10 $ series 5
    print $ largestDiv' 100000 3829
    